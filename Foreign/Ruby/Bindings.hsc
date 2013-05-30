{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances #-}

#include "shim.h"

module Foreign.Ruby.Bindings where

import Foreign
import Foreign.C

-- | This is the type of Ruby values. It is defined as a pointer to some unsigned long, just like Ruby does. The actual value is either pointed to, or encoded in the pointer.
type RValue = Ptr CULong

-- | The Ruby ID type, mostly used for symbols.
type RID = CULong

data ShimDispatch = ShimDispatch String String [RValue]
instance Storable ShimDispatch where
    sizeOf _ = (#size struct s_dispatch)
    alignment = sizeOf
    peek ptr = do a <- peekCString ((#ptr struct s_dispatch, classname) ptr)
                  b <- peekCString ((#ptr struct s_dispatch, methodname) ptr)
                  return (ShimDispatch a b [])
    poke ptr (ShimDispatch c m vals) = do
        cs <- newCString c
        cm <- newCString m
        (#poke struct s_dispatch, classname) ptr cs
        (#poke struct s_dispatch, methodname) ptr cm
        (#poke struct s_dispatch, nbargs) ptr (length vals)
        let arrayblock = (#ptr struct s_dispatch, args) ptr
        pokeArray arrayblock vals

builtinToInt :: RBuiltin -> CULong
builtinToInt RNONE   = 0x00
builtinToInt RNIL    = 0x01
builtinToInt ROBJECT = 0x02
builtinToInt RCLASS  = 0x03
builtinToInt RICLASS = 0x04
builtinToInt RMODULE = 0x05
builtinToInt RFLOAT  = 0x06
builtinToInt RSTRING = 0x07
builtinToInt RREGEXP = 0x08
builtinToInt RARRAY  = 0x09
builtinToInt RFIXNUM = 0x0a
builtinToInt RHASH   = 0x0b
builtinToInt RSTRUCT = 0x0c
builtinToInt RBIGNUM = 0x0d
builtinToInt RFILE   = 0x0e
builtinToInt RTRUE   = 0x20
builtinToInt RFALSE  = 0x21
builtinToInt RDATA   = 0x22
builtinToInt RMATCH  = 0x23
builtinToInt RSYMBOL = 0x24
builtinToInt RBLKTAG = 0x3b
builtinToInt RUNDEF  = 0x3c
builtinToInt RVARMAP = 0x3d
builtinToInt RSCOPE  = 0x3e
builtinToInt RNODE   = 0x3f

intToBuiltin :: CULong -> RBuiltin
-- intToBuiltin 0x00 = RNONE
intToBuiltin 0x01 = RNIL
intToBuiltin 0x02 = ROBJECT
intToBuiltin 0x03 = RCLASS
intToBuiltin 0x04 = RICLASS
intToBuiltin 0x05 = RMODULE
intToBuiltin 0x06 = RFLOAT
intToBuiltin 0x07 = RSTRING
intToBuiltin 0x08 = RREGEXP
intToBuiltin 0x09 = RARRAY
intToBuiltin 0x0a = RFIXNUM
intToBuiltin 0x0b = RHASH
intToBuiltin 0x0c = RSTRUCT
intToBuiltin 0x0d = RBIGNUM
intToBuiltin 0x0e = RFILE
intToBuiltin 0x20 = RTRUE
intToBuiltin 0x21 = RFALSE
intToBuiltin 0x22 = RDATA
intToBuiltin 0x23 = RMATCH
intToBuiltin 0x24 = RSYMBOL
intToBuiltin 0x3b = RBLKTAG
intToBuiltin 0x3c = RUNDEF
intToBuiltin 0x3d = RVARMAP
intToBuiltin 0x3e = RSCOPE
intToBuiltin 0x3f = RNODE
intToBuiltin _ = RNONE

-- | The ruby built-in types
data RBuiltin = RNONE
              | RNIL
              | ROBJECT
              | RCLASS
              | RICLASS
              | RMODULE
              | RFLOAT
              | RSTRING
              | RREGEXP
              | RARRAY
              | RFIXNUM
              | RHASH
              | RSTRUCT
              | RBIGNUM
              | RFILE
              | RTRUE
              | RFALSE
              | RDATA
              | RMATCH
              | RSYMBOL
              | RBLKTAG
              | RUNDEF
              | RVARMAP
              | RSCOPE
              | RNODE
              deriving (Show)

-- | Ruby native types, as encoded in the Value type.
data RType = RFixNum
           | RNil
           | RFalse
           | RTrue
           | RSymbol
           | RBuiltin RBuiltin
           deriving (Show)

type Registered0 = IO RValue
type Registered1 = RValue -> IO RValue
type Registered2 = RValue -> RValue -> IO RValue

-- | Creates a function pointer suitable for usage with `rb_define_global_function` of type `Registered0` (with 0 arguments).
foreign import ccall "wrapper" mkRegistered0 :: Registered0 -> IO (FunPtr Registered0)
-- | Creates a function pointer suitable for usage with `rb_define_global_function` of type `Registered1` (with 1 `RValue` arguments).
foreign import ccall "wrapper" mkRegistered1 :: Registered1 -> IO (FunPtr Registered1)
-- | Creates a function pointer suitable for usage with `rb_define_global_function` of type `Registered2` (with 2 `RValue` arguments).
foreign import ccall "wrapper" mkRegistered2 :: Registered2 -> IO (FunPtr Registered2)

type RegisteredCB3 = RValue -> RValue -> RValue -> IO Int
foreign import ccall "wrapper" mkRegisteredCB3 :: RegisteredCB3 -> IO (FunPtr RegisteredCB3)

foreign import ccall "ruby_init"                 ruby_init                   :: IO ()
foreign import ccall "ruby_finalize"             ruby_finalize               :: IO ()
foreign import ccall "ruby_init_loadpath"        ruby_init_loadpath          :: IO ()
foreign import ccall "rb_str_new2"               c_rb_str_new2               :: CString -> IO RValue
foreign import ccall "rb_load_protect"           c_rb_load_protect           :: RValue -> Int -> Ptr Int -> IO ()
foreign import ccall "rb_funcall"                c_rb_funcall_0              :: RValue -> RID -> Int -> IO RValue
foreign import ccall "rb_funcall"                c_rb_funcall_1              :: RValue -> RID -> Int -> RValue -> IO RValue
foreign import ccall "rb_funcall"                c_rb_funcall_2              :: RValue -> RID -> Int -> RValue -> RValue -> IO RValue
foreign import ccall "rb_funcall"                c_rb_funcall_3              :: RValue -> RID -> Int -> RValue -> RValue -> RValue -> IO RValue
foreign import ccall "rb_funcall"                c_rb_funcall_4              :: RValue -> RID -> Int -> RValue -> RValue -> RValue -> RValue -> IO RValue
foreign import ccall "rb_funcall"                c_rb_funcall_5              :: RValue -> RID -> Int -> RValue -> RValue -> RValue -> RValue -> RValue -> IO RValue
foreign import ccall "rb_gv_get"                 c_rb_gv_get                 :: CString -> IO RValue
foreign import ccall "rb_intern"                 c_rb_intern                 :: CString -> IO RID
foreign import ccall "rb_id2name"                rb_id2name                  :: RID -> IO CString
foreign import ccall "rb_string_value_ptr"       c_rb_string_value_ptr       :: Ptr RValue -> IO CString
foreign import ccall "&rb_cObject"               rb_cObject                  :: Ptr RValue
foreign import ccall "&ruby_errinfo"             ruby_errinfo                :: Ptr RValue
foreign import ccall "rb_define_class"           c_rb_define_class           :: CString -> RValue -> IO RValue
foreign import ccall "rb_define_method"          c_rb_define_method          :: RValue -> CString -> FunPtr a -> Int -> IO ()
foreign import ccall "rb_define_global_function" c_rb_define_global_function :: CString -> FunPtr a -> Int -> IO ()
foreign import ccall "rb_const_get"              rb_const_get                :: RValue -> RID -> IO RValue
foreign import ccall "&safeCall"                 safeCallback                :: FunPtr (RValue -> IO RValue)
foreign import ccall "rb_protect"                c_rb_protect                :: FunPtr (RValue -> IO RValue) -> RValue -> Ptr Int -> IO RValue
foreign import ccall "rb_string_value_cstr"      c_rb_string_value_cstr      :: Ptr RValue -> IO CString
foreign import ccall "rb_ary_new"                rb_ary_new                  :: IO RValue
foreign import ccall "rb_ary_new2"               rb_ary_new2                 :: CLong -> IO RValue
foreign import ccall "rb_ary_new4"               rb_ary_new4                 :: CLong -> Ptr RValue -> IO RValue
foreign import ccall "rb_ary_push"               rb_ary_push                 :: RValue -> RValue -> IO RValue
foreign import ccall "rb_ary_entry"              rb_ary_entry                :: RValue -> CLong -> IO RValue
foreign import ccall "rb_hash_foreach"           rb_hash_foreach             :: RValue -> FunPtr a -> RValue -> IO ()
foreign import ccall "rb_big2str"                rb_big2str                  :: RValue -> CInt -> IO RValue
foreign import ccall "rb_cstr_to_inum"           rb_cstr_to_inum             :: CString -> CInt -> CInt -> IO RValue
foreign import ccall "rb_float_new"              rb_float_new                :: Double -> IO RValue
foreign import ccall "rb_hash_new"               rb_hash_new                 :: IO RValue
foreign import ccall "rb_hash_aset"              rb_hash_aset                :: RValue -> RValue -> RValue -> IO RValue
foreign import ccall "rb_define_module"          c_rb_define_module          :: CString -> IO ()

sym2id :: RValue -> RID
sym2id x = (fromIntegral (ptrToIntPtr x)) `shiftR` 8

id2sym :: RID -> RValue
id2sym x = intPtrToPtr (fromIntegral ( (x `shiftL` 8) .|. 0x0e ))

rbFalse :: RValue
rbFalse = intPtrToPtr 0
rbTrue :: RValue
rbTrue = intPtrToPtr 2
rbNil :: RValue
rbNil = intPtrToPtr 4

rtype :: RValue -> IO RType
rtype rv | ptrToIntPtr rv .&. 1 == 1 = return RFixNum
         | ptrToIntPtr rv  == 0 = return RFalse
         | ptrToIntPtr rv  == 2 = return RTrue
         | ptrToIntPtr rv  == 4 = return RNil
         | ptrToIntPtr rv .&. 0xff == 0x0e = return RSymbol
         | otherwise = fmap (RBuiltin . intToBuiltin . (.&. 0x3f)) (peek rv)


peekArrayLength :: RValue -> IO CLong
peekArrayLength = (#peek struct RArray, len)

peekRFloatValue :: RValue -> IO Double
peekRFloatValue = (#peek struct RFloat, value)

rb_string_value_cstr :: RValue -> IO String
rb_string_value_cstr v = do
    pv <- new v
    o <- c_rb_string_value_cstr pv >>= peekCString
    free pv
    return o

-- | Defines a global function that can be called from the Ruby world.
rb_define_global_function :: String -- ^ Name of the function
                          -> FunPtr a -- ^ Pointer to the function (created with something like `makeRegistered0`, it can only take RValue)
                          -> Int -- ^ Number of arguments the function accepts.
                          -> IO ()
rb_define_global_function s f i = withCString s (\cs -> c_rb_define_global_function cs f i)

rb_define_method :: RValue -> String -> FunPtr a -> Int -> IO ()
rb_define_method r s f i = withCString s (\cs -> c_rb_define_method r cs f i)

rb_define_class :: String  -> RValue -> IO RValue
rb_define_class str rv = withCString str (\s -> c_rb_define_class s rv)

rb_str_new2 :: String -> IO RValue
rb_str_new2 str = withCString str c_rb_str_new2

rb_define_module :: String -> IO ()
rb_define_module str = withCString str c_rb_define_module

rb_load_protect :: String -> Int -> IO Int
rb_load_protect rv a = do
    bptr <- new 0
    rvs <- rb_str_new2 rv
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
    rvp <- new rv
    o <- c_rb_string_value_ptr rvp >>= peekCString
    free rvp
    return o


