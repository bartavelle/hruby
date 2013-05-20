module Foreign.Ruby.Data where

import Foreign.Ruby.Bindings
import Foreign

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
    fmap sequence (forM [0..(nbelems-1)] (\i -> rb_ary_entry v i >>= fromRuby))

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
                readIORef var >>= \x -> case x of
                                            [] -> return Nothing
                                            _ -> return (Just (toHash x))
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
    toRuby (String t) = BS.useAsCString (T.encodeUtf8 t) c_rb_str_new2
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
