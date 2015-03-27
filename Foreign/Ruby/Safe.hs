{-# LANGUAGE LambdaCase, ForeignFunctionInterface #-}
-- | This modules materializes the ruby interpreters as the
-- 'RubyInterpreter' data type. All the calls using these APIs are
-- garanteed to run in the OS thread that the interpreter expects.
module Foreign.Ruby.Safe
    ( -- * Initialization and finalization
      startRubyInterpreter
    , closeRubyInterpreter
    , withRubyInterpreter
    -- * Data types
    , RubyError(..)
    , RValue
    , RubyInterpreter
    -- * Safe variants of other funtions
    , loadFile
    , embedHaskellValue
    , safeMethodCall
    , makeSafe
    , fromRuby
    , toRuby
    , freezeGC
    -- * Wrapping Haskell function and registering them
    , RubyFunction1
    , RubyFunction2
    , RubyFunction3
    , RubyFunction4
    , RubyFunction5
    , registerGlobalFunction1
    , registerGlobalFunction2
    , registerGlobalFunction3
    , registerGlobalFunction4
    , registerGlobalFunction5
    ) where

import Foreign hiding (void)
import qualified Foreign.Ruby.Helpers as FR
import Control.Applicative
import Control.Concurrent
import Control.Exception.Base
import Control.Concurrent.STM
import Control.Monad
import Foreign.Ruby.Bindings

type NoOutput = TMVar (Maybe RubyError)

data IMessage = MsgStop
              | MsgLoadFile !FilePath !NoOutput
              | RegisterGlobalFunction1 !String !RubyFunction1 !NoOutput
              | RegisterGlobalFunction2 !String !RubyFunction2 !NoOutput
              | RegisterGlobalFunction3 !String !RubyFunction3 !NoOutput
              | RegisterGlobalFunction4 !String !RubyFunction4 !NoOutput
              | RegisterGlobalFunction5 !String !RubyFunction5 !NoOutput
              | MakeSafe !(IO ()) !NoOutput

data RubyError = Stack String String
               | WithOutput String RValue
               | OtherError String
               deriving Show

-- | This is actually a newtype around a 'TQueue'.
newtype RubyInterpreter = RubyInterpreter (TQueue IMessage)

-- | All those function types can be used to register functions to the Ruby
-- runtime. Please note that the first argument is always set (it is
-- \"self\"). For this reason, there is no @RubyFunction0@ type.
type RubyFunction1 = RValue -> IO RValue
type RubyFunction2 = RValue -> RValue -> IO RValue
type RubyFunction3 = RValue -> RValue -> RValue -> IO RValue
type RubyFunction4 = RValue -> RValue -> RValue -> RValue -> IO RValue
type RubyFunction5 = RValue -> RValue -> RValue -> RValue -> RValue -> IO RValue

foreign import ccall "wrapper" mkRegisteredRubyFunction1 :: RubyFunction1 -> IO (FunPtr RubyFunction1)
foreign import ccall "wrapper" mkRegisteredRubyFunction2 :: RubyFunction2 -> IO (FunPtr RubyFunction2)
foreign import ccall "wrapper" mkRegisteredRubyFunction3 :: RubyFunction3 -> IO (FunPtr RubyFunction3)
foreign import ccall "wrapper" mkRegisteredRubyFunction4 :: RubyFunction4 -> IO (FunPtr RubyFunction4)
foreign import ccall "wrapper" mkRegisteredRubyFunction5 :: RubyFunction5 -> IO (FunPtr RubyFunction5)

registerGlobalFunction1 :: RubyInterpreter -> String -> RubyFunction1 -> IO (Either RubyError ())
registerGlobalFunction1 int fname f = runMessage_ int (RegisterGlobalFunction1 fname f)
registerGlobalFunction2 :: RubyInterpreter -> String -> RubyFunction2 -> IO (Either RubyError ())
registerGlobalFunction2 int fname f = runMessage_ int (RegisterGlobalFunction2 fname f)
registerGlobalFunction3 :: RubyInterpreter -> String -> RubyFunction3 -> IO (Either RubyError ())
registerGlobalFunction3 int fname f = runMessage_ int (RegisterGlobalFunction3 fname f)
registerGlobalFunction4 :: RubyInterpreter -> String -> RubyFunction4 -> IO (Either RubyError ())
registerGlobalFunction4 int fname f = runMessage_ int (RegisterGlobalFunction4 fname f)
registerGlobalFunction5 :: RubyInterpreter -> String -> RubyFunction5 -> IO (Either RubyError ())
registerGlobalFunction5 int fname f = runMessage_ int (RegisterGlobalFunction5 fname f)

loadFile :: RubyInterpreter -> FilePath -> IO (Either RubyError ())
loadFile int fp = runMessage_ int (MsgLoadFile fp)

-- | Runs an arbitrary computation in the Ruby interpreter thread. This is
-- useful if you want to embed calls from lower level functions. You still
-- need to be careful about the GC's behavior.
makeSafe :: RubyInterpreter -> IO a -> IO (Either RubyError a)
makeSafe int a = do
    -- the IO a computation is embedded in an IO () computation, so that
    -- all is type safe
    mv <- newEmptyMVar
    let embedded = a >>= putMVar mv
    msg <- runMessage_ int (MakeSafe embedded)
    case msg of
        Right _ -> Right <$> takeMVar mv
        Left rr -> return (Left rr)

-- | This transforms any Haskell value into a Ruby big integer encoding the
-- address of the corresponding `StablePtr`. This is useful when you want
-- to pass such values to a Ruby program that will call Haskell functions.
--
-- This is probably a bad idea to do this. The use case is for calling
-- Haskell functions from Ruby, using values generated from the Haskell
-- world. If your main program is in Haskell, you should probably wrap
-- a function partially applied with the value you would want to embed.
embedHaskellValue :: RubyInterpreter -> a -> IO (Either RubyError RValue)
embedHaskellValue int v = makeSafe int $ FR.embedHaskellValue v

-- | A safe version of the corresponding "Foreign.Ruby" function.
safeMethodCall :: RubyInterpreter
               -> String
               -> String
               -> [RValue]
               -> IO (Either RubyError RValue)
safeMethodCall int className methodName args = do
    o <- makeSafe int $ FR.safeMethodCall className methodName args
    case o of
        Left x -> return (Left x)
        Right (Right v) -> return (Right v)
        Right (Left (s,v)) -> return (Left (WithOutput s v))

runMessage_ :: RubyInterpreter -> (NoOutput -> IMessage) -> IO (Either RubyError ())
runMessage_ (RubyInterpreter q) pm = do
    o <- newEmptyTMVarIO
    atomically (writeTQueue q (pm o))
    maybe (Right ()) Left <$> atomically (readTMVar o)

-- | Initializes a Ruby interpreter. This should only be called once. It
-- actually runs an internal server in a dedicated OS thread.
startRubyInterpreter :: IO RubyInterpreter
startRubyInterpreter = do
    q <- newTQueueIO
    void $ forkOS (ruby_init >> ruby_init_loadpath >> go q)
    return (RubyInterpreter q)

{-| This is basically :

> bracket startRubyInterpreter closeRubyInterpreter
-}
withRubyInterpreter :: (RubyInterpreter -> IO a) -> IO a
withRubyInterpreter = bracket startRubyInterpreter closeRubyInterpreter

go :: TQueue IMessage -> IO ()
go q = do
    let continue = return False
        stop     = return True
        runNoOutput :: NoOutput -> IO () -> IO Bool
        runNoOutput no a = do
            try a >>= atomically . putTMVar no . either (\e -> Just $ OtherError $ show (e :: SomeException))
                                                        (const Nothing)
            continue
        runReturns0 :: NoOutput -> IO Int -> String -> IO Bool
        runReturns0 no a errmsg  = do
            s <- try a
            case s of
                Right 0 -> atomically (putTMVar no Nothing)
                Right _ -> do
                    stack <- FR.showErrorStack
                    atomically $ putTMVar no $ Just $ Stack errmsg stack
                Left e -> atomically $ putTMVar no $ Just $ OtherError $ show (e :: SomeException)
            continue

    finished <- atomically (readTQueue q) >>= \case
        MsgStop -> stop
        MsgLoadFile fp mv -> runReturns0 mv (rb_load_protect fp 0)  ("Could not load " ++ fp)
        RegisterGlobalFunction1 fname f no -> runNoOutput no $ mkRegisteredRubyFunction1 f >>= \rf -> rb_define_global_function fname rf 0
        RegisterGlobalFunction2 fname f no -> runNoOutput no $ mkRegisteredRubyFunction2 f >>= \rf -> rb_define_global_function fname rf 1
        RegisterGlobalFunction3 fname f no -> runNoOutput no $ mkRegisteredRubyFunction3 f >>= \rf -> rb_define_global_function fname rf 2
        RegisterGlobalFunction4 fname f no -> runNoOutput no $ mkRegisteredRubyFunction4 f >>= \rf -> rb_define_global_function fname rf 3
        RegisterGlobalFunction5 fname f no -> runNoOutput no $ mkRegisteredRubyFunction5 f >>= \rf -> rb_define_global_function fname rf 4
        MakeSafe a no -> runNoOutput no a
    if finished
        then ruby_finalize
        else go q

-- | This will shut the internal server down.
closeRubyInterpreter :: RubyInterpreter -> IO ()
closeRubyInterpreter (RubyInterpreter q) = atomically (writeTQueue q MsgStop)

-- | Converts a Ruby value to some Haskell type..
fromRuby :: FR.FromRuby a => RubyInterpreter -> RValue -> IO (Either RubyError a)
fromRuby ri rv = either Left (either (Left . OtherError) Right) <$> makeSafe ri (FR.fromRuby rv)

-- | Insert a value in the Ruby runtime. You must always use such
-- a function and the resulting RValue ina 'freezeGC' call.
toRuby :: FR.ToRuby a => RubyInterpreter -> a -> IO (Either RubyError RValue)
toRuby ri = makeSafe ri . FR.toRuby

-- | Runs a computation with the Ruby GC disabled. Once the computation is over, GC will be re-enabled and the `startGC` function run.
freezeGC :: RubyInterpreter -> IO a -> IO a
freezeGC ri c = makeSafe ri (FR.setGC False) *> c <* makeSafe ri (FR.setGC True >> FR.startGC)
