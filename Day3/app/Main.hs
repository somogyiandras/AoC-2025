module Main where

import                  Data.List
import                  Data.Char
import qualified        Control.Monad.State as ST

test_data :: String
test_data = """
987654321111111
811111111111119
234234234234278
818181911112111
"""

third :: (a, b, c) -> c
third (_, _, c) = c

pairs :: String -> [String]
pairs []        = []
pairs (_:[])    = []
pairs (x:y:[])  = [x:y:[]]
pairs cs        = [(cs !! i):(cs !! j):[] | i <- [0..(length cs)-1], j <-[i+1..(length cs)-1]]

max_joltage :: String -> Int
-- max_joltage s = maximum $ map (\ i -> (read i) :: Int) $ filter (\ ss -> length ss == 2) $ subsequences s
-- This naive solution was extra slow because of the big number of combinations
max_joltage ""  = 0
max_joltage s   = maximum $ map (\ i -> (read i) :: Int) $ pairs s

-- 2nd star
-- the new max_joltage function combines the maximum and the substring production
-- instead of pairs it generete the substrings with n length

greatest_joltage :: Int -> String -> Int
greatest_joltage n s = read $ third $ last $ take (n+1) $ iterate go (n, s, "") where
        go :: (Int, String, String) -> (Int, String, String)
        go (n', s', jol) = (n'', s'', jol') where
                (n'', s'') = ST.execState max_battery_in_bank (n', s')
                jol'       = jol ++ singleton (ST.evalState max_battery_in_bank (n', s'))



-- bank is the long string containing digits
max_battery_in_bank :: ST.State (Int, String) Char
max_battery_in_bank = do
        (digits, bank) <- ST.get
        let iresult =
                if length bank > digits
                        then maximum $ map (digitToInt) $ take (length bank - digits + 1) bank
                        else digitToInt $ head bank
        let cresult = intToDigit iresult
        ST.put (digits-1, tail $ dropWhile (/=cresult) bank )
        return $ intToDigit iresult

main :: IO ()
main = do 
        putStrLn "First star"
        putStrLn "Test data"
        putStrLn $ show $ sum $ map max_joltage $ lines test_data


        putStrLn "First star"
        putStrLn "Puzzle data"
        puzzle_data <- readFile "input.txt"
        putStrLn $ show $ sum $ map max_joltage $ lines puzzle_data


        putStrLn "\n\nSecond star"
        putStrLn "Test data"
        putStrLn $ show $ sum $ map (greatest_joltage 12) $ lines test_data

        putStrLn "First star"
        putStrLn "Puzzle data"
        putStrLn $ show $ sum $ map (greatest_joltage 12) $ lines puzzle_data
