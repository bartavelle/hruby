module Main where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import Foreign.Ruby
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Monadic

subvalue :: Gen Value
subvalue = frequency [(50, s), (20, b), (20, n), (1, h), (1, a)]

str :: Gen T.Text
str = fmap T.pack (listOf (elements (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'])))

s :: Gen Value
s = fmap String str

a :: Gen Value
a = fmap (Array . V.fromList) (resize 100 (listOf subvalue))

b :: Gen Value
b = fmap Bool arbitrary

n :: Gen Value
n = fmap (Number . fromIntegral) (arbitrary :: Gen Integer)

h :: Gen Value
h = fmap object $ do
  k <- listOf str
  v <- listOf subvalue
  return (zip k v)

avalue :: Gen Value
avalue = frequency [(1, s), (5, a), (1, b), (1, n), (5, h)]

roundTrip :: RubyInterpreter -> Property
roundTrip i = monadicIO $ do
  v <- pick avalue
  ex <- run $
    freezeGC i $ do
      rub <- toRuby i v >>= either (error . show) return
      nxt <- safeMethodCall i "TestClass" "testfunc" [rub]
      case nxt of
        Right x -> do
          out <-
            fromRuby i x >>= \r -> case r of
              Right r' -> return r'
              Left rr -> error (show rr)
          when (out /= v) (print out)
          return (v == out)
        Left rr -> print rr >> return False
  assert ex

main :: IO ()
main = withRubyInterpreter $ \i -> do
  loadFile i "./test/test.rb" >>= either (error . show) return
  result <- quickCheckWithResult (stdArgs {maxSuccess = 1000}) (roundTrip i)
  unless (isSuccess result) exitFailure
