module Tests (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Main

test_data :: String
test_data = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224, 1698522-1698528,446443-446449,38593856-38593862,565653-565659, 824824821-824824827,2121212118-2121212124"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Test split by separator" $
      breakAt ',' "Hello, Haskell!,separator , ,"  @?= ["Hello", " Haskell!", "separator ", " ", ""] ]
