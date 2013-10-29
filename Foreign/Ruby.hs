-- | This is the main module of this library. Other functionnalities are
-- exposed in "Foreign.Ruby.Bindings" and "Foreign.Ruby.Helpers", but this
-- should be enough for most cases.
module Foreign.Ruby
    ( -- * Initialization / cleanup
      initialize
    , finalize
    -- * Converting from and to Ruby values
    , RValue
    , RID
    , FromRuby (fromRuby)
    , ToRuby (toRuby)
    , getSymbol
    -- * Callbacks
    -- | These functions could be used to call Haskell functions from the Ruby
    -- world. While fancier stuff could be achieved by tapping into
    -- "Foreign.Ruby.Bindings", those methods should be easy to use and should
    -- cover most use cases.
    --
    -- The `embedHaskellValue`, `extractHaskellValue` and
    -- `freeHaskellValue` functions are very unsafe, and should be used only in very
    -- controlled environments.
    , embedHaskellValue
    , extractHaskellValue
    , freeHaskellValue
    , mkRegistered0
    , mkRegistered1
    , mkRegistered2
    , rb_define_global_function
    -- * GC control
    -- | This is a critical part of embedding the Ruby interpreter. Data
    -- created using the `toRuby` function might be collected at any time.
    -- For now, the solution is to disable the GC mechanism during calls to
    -- Ruby functions. If someone knows of a better solution, please
    -- contact the author of this library.
    , setGC
    , startGC
    , freezeGC
    -- * Interacting with the interpreter
    , rb_load_protect
    , safeMethodCall
    -- * Error handling
    , showErrorStack
    -- * Various
    , rb_iv_set
    )
where

import Foreign.Ruby.Helpers
import Foreign.Ruby.Bindings

-- | You must run this before anything else.
initialize :: IO ()
initialize = ruby_init >> ruby_init_loadpath

-- | You might want to run this when you are done with all the Ruby stuff.
finalize :: IO ()
finalize = ruby_finalize

-- | Gets the `RValue` correponding to the given named symbol.
getSymbol :: String -> IO RValue
getSymbol = fmap id2sym . rb_intern

