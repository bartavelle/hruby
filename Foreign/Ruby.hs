-- | The embedded Ruby interpreter must run on its own thread. The functions
-- in this module should enforce this property. For lower level access,
-- please look at "Foreign.Ruby.Bindings" and "Foreign.Ruby.Helpers".
--
-- > withRubyInterpreter $ \i -> do
-- >   dsqsddqs
module Foreign.Ruby
( -- * Initialization / cleanup
    RubyInterpreter
  , startRubyInterpreter
  , closeRubyInterpreter
  , withRubyInterpreter
  -- * Running Ruby actions
  , loadFile
  , Foreign.Ruby.Safe.embedHaskellValue
  , Foreign.Ruby.Safe.safeMethodCall
  , makeSafe
  , RubyError(..)
  -- * Converting to and from Ruby values
  , RValue
  , RID
  , Foreign.Ruby.Safe.fromRuby
  , Foreign.Ruby.Safe.toRuby
  , Foreign.Ruby.Safe.freezeGC
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
  , freeHaskellValue
  , extractHaskellValue
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
  )
where

import Foreign.Ruby.Safe
import Foreign.Ruby.Helpers
import Foreign.Ruby.Bindings

-- | Gets the `RValue` correponding to the given named symbol.
getSymbol :: String -> IO RValue
getSymbol = fmap id2sym . rb_intern
