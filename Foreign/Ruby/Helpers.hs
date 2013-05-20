module Foreign.Ruby.Helpers where

import Foreign.Ruby.Bindings
import Foreign
import Foreign.C

embedHaskellValue :: a -> IO RValue
embedHaskellValue = fmap (castPtr . castStablePtrToPtr) . newStablePtr

freeHaskellValue :: RValue -> IO ()
freeHaskellValue = freeStablePtr . castPtrToStablePtr . castPtr

extractHaskellValue :: RValue -> IO a
extractHaskellValue = deRefStablePtr . castPtrToStablePtr . castPtr

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
            r <- showError
            ruby_finalize
            return (Left r)

defineGlobalClass :: String -> IO RValue
defineGlobalClass s = peek rb_cObject >>= rb_define_class s

safeMethodCall :: String -> String -> [RValue] -> IO (Either String RValue)
safeMethodCall classname methodname args =
    if length args > 16
        then return (Left "too many arguments")
        else do
            dispatch <- malloc
            poke dispatch (ShimDispatch classname methodname args)
            pstatus <- malloc
            o <- c_rb_protect safeCallback (castPtr dispatch) pstatus
            status <- peek pstatus
            free dispatch
            free pstatus
            if status == 0
                then return (Right o)
                else fmap Left showError

showError :: IO String
showError = do
    a <- rb_gv_get "$!"
    b <- rb_intern "message"
    o <- rb_funcall a b []
    rb_string_value_ptr o


