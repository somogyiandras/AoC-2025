module Tests (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Main


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Test split by separator" $
      breakAt ',' "Hello, Haskell!,separator , ,"  @?= ["Hello", " Haskell!", "separator ", " ", ""] ]
