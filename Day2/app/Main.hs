module Main where

import          Control.Arrow
import          Data.List       (inits, tails, isPrefixOf, (\\))
import          Data.List.Split (splitOn)

test_data :: String
test_data="11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

{-
breakAt :: String -> Char -> [String]
breakAt s sep = case dropWhile (==sep) s of
                                "" -> []
                                s' -> w : breakAt s'' sep
                                      where (w, s'') = break (==sep) s'
-}

parseRange :: String -> [Int]
parseRange s = map read $ splitOn "-" s

parseData :: String -> [Int]
parseData st = concat $
        foldr
                ( \ s acc -> makeRange (parseRange s) : acc )
                []
                (splitOn "," st) where
                        makeRange (mi:ma:[]) = [mi..ma]

invalidID :: Int -> Bool
invalidID i 
        | odd l         = False
        | otherwise     = i `div` (10^k) == i `mod` (10^k)
                where   l = length $ show i
                        k = l `div` 2

invalidID2 :: Int -> Bool
invalidID2 n = any ( \ (r, s) -> r `isRepeats` s) parts
        where
                parts = filter (\(a,b) -> a /= "" && b/="") $ uncurry zip $ (inits &&& tails) nn
                nn = show n

-- $> invalidID2 11


isRepeats :: String -> String -> Bool
isRepeats _ ""                  = True
isRepeats "" _                  = False
isRepeats re s
        | length re > length s  = False
        | re `isPrefixOf` s     = True && (isRepeats re $ s \\ re)
        | otherwise             = False

-- $> "12" `isRepeats` "1212121"


main :: IO ()
main = do
        putStrLn "First star"
        putStrLn "Test data"
        putStrLn $ show $ sum $ filter invalidID $ parseData test_data

        putStrLn "First star"
        putStrLn "Puzzle data"
        puzzle_data <- readFile "input.txt"
        putStrLn $ show $ sum $ filter invalidID $ parseData puzzle_data

        putStrLn "\n\nSecond star"
        putStrLn "Test data"
        putStrLn $ show $ sum $ filter invalidID2 $ parseData test_data

        putStrLn "First star"
        putStrLn "Puzzle data"
        puzzle_data <- readFile "input.txt"
        putStrLn $ show $ sum $ filter invalidID2 $ parseData puzzle_data
