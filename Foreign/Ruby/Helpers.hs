{-# LANGUAGE OverloadedStrings #-}
module Foreign.Ruby.Helpers where

import Foreign.Ruby.Bindings

import Foreign
import Data.Aeson
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Scientific

-- | The class of things that can be converted from Ruby values. Note that
-- there are a ton of stuff that are Ruby values, hence the `Maybe` type,
-- as the instances will probably be incomplete.
class FromRuby a where
    -- | To define more instances, please look at the instances defined in
    -- "Foreign.Ruby.Helpers".
    fromRuby :: RValue -> IO (Either String a)

-- | Whenever you use `ToRuby`, don't forget to use something like
-- `freezeGC` or you will get random segfaults.
class ToRuby a where
    toRuby   :: a -> IO RValue

fromRubyIntegral :: Integral n => RValue -> IO (Either String n)
fromRubyIntegral = fmap (Right . fromIntegral) . num2long
toRubyIntegral :: Integral n => n -> IO RValue
toRubyIntegral = int2num . fromIntegral

fromRubyArray :: FromRuby a => RValue -> IO (Either String [a])
fromRubyArray v = do
    nbelems <- arrayLength v
    fmap sequence (forM [0..(nbelems-1)] (rb_ary_entry v >=> fromRuby))

instance FromRuby a => FromRuby [a] where
    fromRuby v = do
        t <- rtype v
        case t of
            RBuiltin RARRAY -> fromRubyArray v
            _ -> return $ Left ("not an array! " ++ show t)

instance ToRuby a => ToRuby [a] where
    toRuby lst = do
        vals <- mapM toRuby lst
        Foreign.withArray vals (rb_ary_new4 (fromIntegral (length lst)))

instance FromRuby BS.ByteString where
    fromRuby v = do
        t <- rtype v
        case t of
            RBuiltin RSTRING -> do
                pv <- new v
                cstr <- c_rb_string_value_cstr pv
                free pv
                fmap Right (BS.packCString cstr)
            RSymbol -> fmap Right (rb_id2name (sym2id v) >>= BS.packCString)
            _ -> return (Left ("Expected a string, not " ++ show t))
instance ToRuby BS.ByteString where
    toRuby s = BS.useAsCString s c_rb_str_new2

instance FromRuby T.Text where
    fromRuby = fmap (fmap T.decodeUtf8) . fromRuby
instance ToRuby T.Text where
    toRuby = toRuby . T.encodeUtf8

instance ToRuby Double where
    toRuby = newFloat
instance FromRuby Double where
    fromRuby = fmap Right . num2dbl

instance FromRuby Integer where
    fromRuby = fromRubyIntegral
instance ToRuby Integer where
    toRuby = toRubyIntegral
instance FromRuby Int where
    fromRuby = fromRubyIntegral
instance ToRuby Int where
    toRuby = toRubyIntegral

-- | This is the most complete instance that is provided in this module.
-- Please note that it is far from being sufficient for even basic
-- requirements. For example, the `Value` type can only encode
-- dictionnaries with keys that can be converted to strings.
instance FromRuby Value where
    fromRuby v = do
        t <- rtype v
        case t of
            RFixNum          -> fmap (fmap (Number . (fromIntegral :: Integer -> Scientific))) (fromRuby v)
            RNil             -> return (Right Null)
            RFalse           -> return (Right (Bool False))
            RTrue            -> return (Right (Bool True))
            RSymbol          -> fmap (fmap (String . T.decodeUtf8)) (fromRuby v)
            RBuiltin RNIL    -> return (Right Null)
            RBuiltin RSTRING -> fmap (fmap (String . T.decodeUtf8)) (fromRuby v)
            RBuiltin RARRAY  -> fmap (fmap (Array . V.fromList)) (fromRubyArray v)
            RBuiltin RTRUE   -> return (Right (Bool True))
            RBuiltin RFALSE  -> return (Right (Bool False))
            RBuiltin RUNDEF  -> return (Right Null)
            RBuiltin RFLOAT  -> fmap (fmap (Number . fromRational . (toRational :: Double -> Rational))) (fromRuby v)
            RBuiltin RBIGNUM -> do
                bs <- rb_big2str v 10 >>= fromRuby
                return $ case fmap BS.readInteger bs of
                    Right (Just (x,"")) -> Right (Number (fromIntegral x))
                    Right _ -> Left ("Expected an integer, not " ++ show bs)
                    Left rr -> Left rr
            RBuiltin RNONE -> return (Right Null)
            RBuiltin RHASH   -> do
                var <- newIORef []
                let appender :: RValue -> RValue -> RValue -> IO Int
                    appender key val _ = do
                        vvar <- readIORef var
                        vk <- fromRuby key
                        vv <- fromRuby val
                        case (vk, vv) of
                            (Right jk, Right jv) -> writeIORef var ( (jk,jv) : vvar ) >> return 0
                            _ -> return 1
                    toHash = Object . HM.fromList
                wappender <- mkRegisteredCB3 appender
                rb_hash_foreach v wappender rbNil
                freeHaskellFunPtr wappender
                fmap (Right . toHash) (readIORef var)
            _ -> return $ Left ("Could not decode: " ++ show t)

instance ToRuby Scientific where
    toRuby s | base10Exponent s >= 0 = toRuby (coefficient s)
             | otherwise = toRuby (fromRational (toRational s) :: Double)

instance ToRuby Value where
    toRuby (Number x) = toRuby x
    toRuby (String t) = let bs = T.encodeUtf8 t
                        in  BS.useAsCString bs c_rb_str_new2
    toRuby Null = return rbNil
    toRuby (Bool True) = return rbTrue
    toRuby (Bool False) = return rbFalse
    toRuby (Array ar) = toRuby (V.toList ar)
    toRuby (Object m) = do
        hash <- rb_hash_new
        forM_ (HM.toList m) $ \(k, v) -> do
            rk <- toRuby k
            rv <- toRuby v
            rb_hash_aset hash rk rv
        return hash

-- | An unsafe version of the corresponding "Foreign.Ruby.Safe" function.
embedHaskellValue :: a -> IO RValue
embedHaskellValue v = do
    intptr <- fmap (fromIntegral . ptrToIntPtr . castStablePtrToPtr) (newStablePtr v) :: IO Integer
    toRuby intptr

-- | Frees the Haskell value represented by the corresponding `RValue`.
-- This is probably extremely unsafe to do, and will most certainly lead to
-- exploitable security bug if you use something modified from Ruby land.
-- You should always free the `RValue` you generated from
-- `embedHaskellValue`.
freeHaskellValue :: RValue -> IO ()
freeHaskellValue v = do
    intptr <- fromRuby v :: IO (Either String Integer)
    case intptr of
        Right i -> freeStablePtr (castPtrToStablePtr (intPtrToPtr (fromIntegral i)))
        Left rr -> error ("Could not decode embedded value during free! " ++ rr)

-- | This is unsafe as hell, so you'd better be certain this RValue has not
-- been tempered with : GC frozen, bugfree Ruby scripts.
--
-- If it has been tempered by an attacker, you are probably looking at
-- a good vector for arbitrary code execution.
extractHaskellValue :: RValue -> IO a
extractHaskellValue v = do
    intptr <- fromRuby v :: IO (Either String Integer)
    case intptr of
        Right i -> deRefStablePtr (castPtrToStablePtr (intPtrToPtr (fromIntegral i)))
        Left rr -> error ("Could not decode embedded value! " ++ rr)

runscript :: String -> IO (Either String ())
runscript filename = do
    ruby_initialization
    status <- rb_load_protect filename 0
    if status == 0
        then ruby_finalize >> return (Right ())
        else do
            r <- showErrorStack
            ruby_finalize
            return (Left r)

defineGlobalClass :: String -> IO RValue
defineGlobalClass s = peek rb_cObject >>= rb_define_class s

-- | Runs a Ruby method, capturing errors.
safeMethodCall :: String -- ^ Class name.
               -> String -- ^ Method name.
               -> [RValue] -- ^ Arguments. Please note that the maximum number of arguments is 16.
               -> IO (Either (String, RValue) RValue) -- ^ Returns either an error message / value couple, or the value returned by the function.
safeMethodCall classname methodname args =
    if length args > 16
        then return (Left ("too many arguments", rbNil))
        else do
            dispatch <- new (ShimDispatch classname methodname args)
            pstatus <- new 0
            o <- c_rb_protect safeCallback (castPtr dispatch) pstatus
            status <- peek pstatus
            free dispatch
            free pstatus
            if status == 0
                then return (Right o)
                else do
                    err <- showErrorStack
                    return (Left (err,o))

-- | Gives a (multiline) error friendly string representation of the last
-- error.
showErrorStack :: IO String
showErrorStack = do
    runtimeerror <- rb_gv_get "$!"
    m <- if runtimeerror == rbNil
             then return "Unknown runtime error"
             else do
                 message <- rb_intern "message"
                 fmap (either T.pack id) (rb_funcall runtimeerror message [] >>= fromRuby)
    rbt <- rb_gv_get "$@"
    bt <- if rbt == rbNil
              then return []
              else fmap (either (const []) id) (fromRuby rbt)
    return (T.unpack (T.unlines (m : bt)))

-- | Sets the current GC operation. Please note that this could be modified
-- from Ruby scripts.
setGC :: Bool -- ^ Set to `True` to enable GC, and to `False` to disable it.
      -> IO (Either (String, RValue) RValue)
setGC nw = do
    let method = if nw
                     then "enable"
                     else "disable"
    safeMethodCall "GC" method []

-- | Runs the Ruby garbage collector.
startGC :: IO ()
startGC = Control.Monad.void (safeMethodCall "GC" "start" [])

-- | Runs a computation with the Ruby GC disabled. Once the computation is over, GC
-- will be re-enabled and the `startGC` function run.
freezeGC :: IO a -> IO a
freezeGC computation = do
    Control.Monad.void (setGC False)
    o <- computation
    Control.Monad.void (setGC True)
    startGC
    return o

