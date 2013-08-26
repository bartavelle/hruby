module Main where

import Foreign.Ruby.Helpers
import Foreign.Ruby.Bindings
import Data.Aeson
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Text as T
import Data.Attoparsec.Number
import qualified Data.Vector as V

instance Arbitrary Number where
    arbitrary = frequency [ (1, fmap D arbitrary) , (1, fmap I arbitrary) ]

subvalue :: Gen Value
subvalue = frequency [(50,s),(20,b),(20,n),(1,h),(1,a)]

str :: Gen T.Text
str = fmap T.pack (listOf (elements (['A'..'Z'] ++ ['a' .. 'z'] ++ ['0'..'9'])))

s :: Gen Value
s = fmap String str

a :: Gen Value
a = fmap (Array . V.fromList) (resize 100 (listOf subvalue))

b :: Gen Value
b = fmap Bool arbitrary

n :: Gen Value
n = fmap Number arbitrary

h :: Gen Value
h = fmap object $ do
        k <- listOf str
        v <- listOf subvalue
        return (zip k v)

instance Arbitrary Value where
    arbitrary = frequency [(1,s),(5,a),(1,b),(1,n),(5,h)]

roundTrip :: Property
roundTrip = monadicIO $ do
    v <- pick (arbitrary :: Gen Value)
    void (run (setGC False))
    rub <- run (toRuby v)
    nxt <- run (safeMethodCall "TestClass" "testfunc" [rub])
    case nxt of
        Right x -> do
            out <- run (fromRuby x)
            void (run (setGC True))
            run startGC
            when (out /= Just v) (run (print out))
            assert (Just v == out)
        Left (rr,_) -> run (print rr) >> assert False

main :: IO ()
main = do
    ruby_init
    ruby_init_loadpath
    rb_define_module "test"
    st <- rb_load_protect "./test/test.rb" 0
    unless (st == 0) (showErrorStack >>= error)
    quickCheckWith (stdArgs { maxSuccess = 1000 } ) roundTrip
    ruby_finalize
