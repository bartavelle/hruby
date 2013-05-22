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
    arbitrary = do
        t <- choose (0,1) :: Gen Int
        case t of
            0 -> fmap D arbitrary
            _ -> fmap I arbitrary

subvalue :: Gen Value
subvalue = frequency [(100,s),(10,b),(50,n),(1,h),(1,a)]

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
    arbitrary = frequency [(100,s),(5,a),(10,b),(50,n),(10,h)]

roundTrip :: Property
roundTrip = monadicIO $ do
    v <- pick (arbitrary :: Gen Value)
    run (print v)
    rub <- run (toRuby v)
    run $ putStrLn "toRuby"
    nxt <- run (safeMethodCall "TestClass" "testfunc" [rub])
    run $ putStrLn "call"
    case nxt of
        Right x -> do
            out <- run (fromRuby x)
            run (print out)
            assert (Just v == out)
        Left (rr,_) -> run (print rr) >> assert (1 == 2)

main :: IO ()
main = do
    ruby_init
    ruby_init_loadpath
    s <- rb_load_protect "test/test.rb" 0
    unless (s == 0) (showErrorStack >>= error)
    quickCheckWith (stdArgs { maxSuccess = 1000 } ) roundTrip
    ruby_finalize
