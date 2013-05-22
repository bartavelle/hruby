module Foreign.Ruby.Helpers where

import Foreign.Ruby.Bindings

import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C
import Data.Aeson
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.Number
import qualified Data.Vector as V
import Data.IORef
import qualified Data.HashMap.Strict as HM

class FromRuby a where
    fromRuby :: RValue -> IO (Maybe a)
class ToRuby a where
    toRuby   :: a -> IO RValue

fromRubyIntegral :: Integral n => RValue -> IO (Maybe n)
fromRubyIntegral r = do
    t <- rtype r
    case t of
        RFixNum -> return (Just (fromIntegral (ptrToIntPtr r `shiftR` 1)))
        _ -> return Nothing
toRubyIntegral :: Integral n => n -> IO RValue
toRubyIntegral n = return (intPtrToPtr ( (fromIntegral n `shiftL` 1) .|. 1))

fromRubyArray :: FromRuby a => RValue -> IO (Maybe [a])
fromRubyArray v = do
    nbelems <- peekArrayLength v
    fmap sequence (forM [0..(nbelems-1)] (rb_ary_entry v >=> fromRuby))

instance FromRuby a => FromRuby [a] where
    fromRuby v = do
        t <- rtype v
        case t of
            RBuiltin RARRAY -> fromRubyArray v
            _ -> return Nothing
instance ToRuby a => ToRuby [a] where
    toRuby lst = do
        arr <- rb_ary_new2 (fromIntegral (length lst))
        mapM_ (toRuby >=> rb_ary_push arr) lst
        return arr

instance FromRuby BS.ByteString where
    fromRuby v = do
        t <- rtype v
        case t of
            RBuiltin RSTRING -> do
                pv <- new v
                cstr <- c_rb_string_value_cstr pv
                free pv
                fmap Just (BS.packCString cstr)
            RSymbol -> fmap Just (rb_id2name (sym2id v) >>= BS.packCString)
            _ -> return Nothing
instance ToRuby BS.ByteString where
    toRuby s = BS.useAsCString s c_rb_str_new2

instance FromRuby T.Text where
    fromRuby = fmap (fmap T.decodeUtf8) . fromRuby
instance ToRuby T.Text where
    toRuby = toRuby . T.encodeUtf8

instance FromRuby Integer where
    fromRuby = fromRubyIntegral
instance ToRuby Integer where
    toRuby = toRubyIntegral
instance FromRuby Int where
    fromRuby = fromRubyIntegral
instance ToRuby Int where
    toRuby = toRubyIntegral

instance FromRuby Value where
    fromRuby v = do
        t <- rtype v
        case t of
            RFixNum          -> fmap (fmap (Number . I)) (fromRuby v)
            RNil             -> return (Just Null)
            RFalse           -> return (Just (Bool False))
            RTrue            -> return (Just (Bool True))
            RSymbol          -> fmap (fmap (String . T.decodeUtf8)) (fromRuby v)
            RBuiltin RNIL    -> return (Just Null)
            RBuiltin RSTRING -> fmap (fmap (String . T.decodeUtf8)) (fromRuby v)
            RBuiltin RARRAY  -> fmap (fmap (Array . V.fromList)) (fromRubyArray v)
            RBuiltin RTRUE   -> return (Just (Bool True))
            RBuiltin RFALSE  -> return (Just (Bool False))
            RBuiltin RUNDEF  -> return (Just Null)
            RBuiltin RFLOAT  -> fmap (Just . Number . D) (peekRFloatValue v)
            RBuiltin RBIGNUM -> do
                bs <- rb_big2str v 10 >>= fromRuby
                case fmap BS.readInteger bs of
                    Just (Just (x,"")) -> return (Just (Number (I x)))
                    _ -> return Nothing
            RBuiltin RHASH   -> do
                var <- newIORef []
                let appender :: RValue -> RValue -> RValue -> IO Int
                    appender key val _ = do
                        vvar <- readIORef var
                        vk <- fromRuby key
                        vv <- fromRuby val
                        case (vk, vv) of
                            (Just jk, Just jv) -> writeIORef var ( (jk,jv) : vvar ) >> return 0
                            _ -> writeIORef var [] >> return 1
                    toHash = Object . HM.fromList
                wappender <- mkRegisteredCB3 appender
                rb_hash_foreach v wappender rbNil
                freeHaskellFunPtr wappender
                fmap (Just . toHash) (readIORef var)
            _ -> do
                putStrLn ("Could not decode: " ++ show t)
                return Nothing

instance ToRuby Value where
    toRuby (Number (I x)) = do
        let maxRubyInt = fromIntegral (maxBound :: IntPtr) `shiftR` 1 :: Integer
        if x >= maxRubyInt
            then BS.useAsCString (BS.pack (show x)) (\cs -> rb_cstr_to_inum cs 10 0)
            else toRubyIntegral x
    toRuby (Number (D x)) = rb_float_new (uncurry encodeFloat (decodeFloat x))
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

embedHaskellValue :: a -> IO RValue
embedHaskellValue v = do
    intptr <- fmap (fromIntegral . ptrToIntPtr . castStablePtrToPtr) (newStablePtr v) :: IO Integer
    toRuby intptr

freeHaskellValue :: RValue -> IO ()
freeHaskellValue v = do
    intptr <- fromRuby v :: IO (Maybe Integer)
    case intptr of
        Just i -> freeStablePtr (castPtrToStablePtr (intPtrToPtr (fromIntegral i)))
        Nothing -> error "Could not decode embedded value during free!"

extractHaskellValue :: RValue -> IO a
extractHaskellValue v = do
    intptr <- fromRuby v :: IO (Maybe Integer)
    case intptr of
        Just i -> deRefStablePtr (castPtrToStablePtr (intPtrToPtr (fromIntegral i)))
        Nothing -> error "Could not decode embedded value!"

rb_string_value_cstr :: RValue -> IO String
rb_string_value_cstr v = do
    pv <- new v
    o <- c_rb_string_value_cstr pv >>= peekCString
    free pv
    return o

rb_define_global_function :: String -> FunPtr a -> Int -> IO ()
rb_define_global_function s f i = withCString s (\cs -> c_rb_define_global_function cs f i)

rb_define_method :: RValue -> String -> FunPtr a -> Int -> IO ()
rb_define_method r s f i = withCString s (\cs -> c_rb_define_method r cs f i)

rb_define_class :: String  -> RValue -> IO RValue
rb_define_class str rv = withCString str (\s -> c_rb_define_class s rv)

rb_str_new2 :: String -> IO RValue
rb_str_new2 str = withCString str c_rb_str_new2

rb_load_protect :: String -> Int -> IO Int
rb_load_protect rv a = do
    bptr <- malloc
    rvs <- rb_str_new2 rv
    poke bptr 0
    c_rb_load_protect rvs a bptr
    status <- peek bptr
    free bptr
    return status

rb_funcall :: RValue -> RID -> [RValue] -> IO RValue
rb_funcall a b []          = c_rb_funcall_0 a b 0
rb_funcall a b [d]         = c_rb_funcall_1 a b 1 d
rb_funcall a b [d,e]       = c_rb_funcall_2 a b 2 d e
rb_funcall a b [d,e,f]     = c_rb_funcall_3 a b 3 d e f
rb_funcall a b [d,e,f,g]   = c_rb_funcall_4 a b 4 d e f g
rb_funcall a b [d,e,f,g,h] = c_rb_funcall_5 a b 5 d e f g h
rb_funcall _ _ _           = error "Can't call functions with that many arguments"

rbMethodCall :: String -> String -> [RValue] -> IO RValue
rbMethodCall classname methodname args = do
    c <- getClass classname
    m <- rb_intern methodname
    rb_funcall c m args

getClass :: String -> IO RValue
getClass s = do
    i <- rb_intern s
    o <- peek rb_cObject
    rb_const_get o i

rb_gv_get :: String -> IO RValue
rb_gv_get s = withCString s c_rb_gv_get

rb_intern :: String -> IO RID
rb_intern s = withCString s c_rb_intern

rb_string_value_ptr :: RValue -> IO String
rb_string_value_ptr rv = do
    rvp <- malloc
    poke rvp rv
    o <- c_rb_string_value_ptr rvp >>= peekCString
    free rvp
    return o

runscript :: String -> IO (Either String ())
runscript filename = do
    ruby_init
    ruby_init_loadpath
    status <- rb_load_protect filename 0
    if status == 0
        then ruby_finalize >> return (Right ())
        else do
            r <- showErrorStack
            ruby_finalize
            return (Left r)

defineGlobalClass :: String -> IO RValue
defineGlobalClass s = peek rb_cObject >>= rb_define_class s

safeMethodCall :: String -> String -> [RValue] -> IO (Either (String, RValue) RValue)
safeMethodCall classname methodname args =
    if length args > 16
        then return (Left ("too many arguments", rbNil))
        else do
            dispatch <- malloc
            poke dispatch (ShimDispatch classname methodname args)
            pstatus <- malloc
            poke pstatus 0
            o <- c_rb_protect safeCallback (castPtr dispatch) pstatus
            status <- peek pstatus
            free dispatch
            free pstatus
            if status == 0
                then return (Right o)
                else do
                    err <- showErrorStack
                    return (Left (err,o))

showErrorStack :: IO String
showErrorStack = do
    runtimeerror <- rb_gv_get "$!"
    m <- if runtimeerror == rbNil
             then return "Unknown runtime error"
             else do
                 message <- rb_intern "message"
                 fmap (fromMaybe "undeserializable error") (rb_funcall runtimeerror message [] >>= fromRuby)
    rbt <- rb_gv_get "$@"
    bt <- if rbt == rbNil
              then return []
              else fmap (fromMaybe []) (fromRuby rbt)
    return (T.unpack (T.unlines (m : bt)))


