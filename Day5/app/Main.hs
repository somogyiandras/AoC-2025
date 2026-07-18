module Main where

import qualified        Data.Set        as S
import                  Data.Interval
import qualified        Data.Interval.Borel as Borel
import                  Data.Maybe


testFile :: FilePath
testFile = "Day_5_test_data.txt"

puzzleFile :: FilePath
puzzleFile = "Day_5_puzzle_data.txt"

------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------

countS :: (a -> Bool) -> S.Set a -> Int
countS f = S.size . (S.filter f)

count :: (a -> Bool) -> [a] -> Int
count f = length . (filter f)

-- read range from string to set
rangeSetFromString :: String -> S.Set Int
rangeSetFromString input = let
        (startS, endS) = break (=='-') input
        in S.fromDistinctAscList [read startS .. read $ tail endS]

-- read range from string to interval
rangeIFromString :: String -> Interval Int
rangeIFromString input = let
        (startS, endS) = break (=='-') input
        in read startS :||: (read $ tail endS)



-- content as lines of strings
-- split content at empty line
splitContent :: [String] -> ([String], [String])
splitContent content = let
        (firstPart, secondPart) = break (=="") content
        in (firstPart, tail secondPart)


main :: IO ()
main = do
-- this part is obsolete (too slow for the puzzle)
-- leave it to remember this idea
        putStr "First star - Test data - Answer: "
        test_data  <- readFile testFile
        let
                (rangesS, ingredientsS) = splitContent $ lines test_data
                freshRanges = S.unions $ map rangeSetFromString rangesS
                ingredients = S.fromList $ map read ingredientsS
                answer = countS (\ing -> ing `S.member` freshRanges) ingredients
                in putStrLn $ show $ answer

-- solve again using intervals
--
        putStr "First star - Test data - Intervals - Answer: "
        test_data  <- readFile testFile
        let
                (rangesS, ingredientsS) = splitContent $ lines test_data
                freshRanges = unions $ map rangeIFromString rangesS
                size = sum $ map ( (1+) . fromJust . measure) freshRanges
                ingredients = map read ingredientsS
                answer = count (\ing -> ing `Borel.member` Borel.borel freshRanges) ingredients
                in putStrLn $ (show answer) ++ "\nSecond star - Test data - Answer: " ++ (show size)

        putStr "First star - Puzzle data - Intervals - Answer: "
        puzzle_data  <- readFile puzzleFile
        let
                (rangesS, ingredientsS) = splitContent $ lines puzzle_data
                freshRanges = unions $ map rangeIFromString rangesS
                size = sum $ map ( (1+) . fromJust . measure) freshRanges
                ingredients = map read ingredientsS
                answer = count (\ing -> ing `Borel.member` Borel.borel freshRanges) ingredients
                in putStrLn $ (show answer) ++ "\nSecond star - Puzzle data - Answer: " ++ (show size)


