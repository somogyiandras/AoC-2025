module Main where

import                  Data.List (transpose, intercalate, unfoldr)
import                  Text.ParserCombinators.ReadP
import                  Text.Read (readMaybe)

testFile :: FilePath
testFile = "Day_6_test_data.txt"

puzzleFile :: FilePath
puzzleFile = "Day_6_puzzle_data.txt"

programMatrix :: [(FilePath, String, Parser, Solver)]
programMatrix =
        [
        (testFile,      "1st star - Test: ",    parser,         solve1star)
      , (puzzleFile,    "1st star - Puzzle: ",  parser,         solve1star) 
      , (testFile,      "2nd star - Test: ",    parser2,        solve1star) 
      , (puzzleFile,    "2nd star - Puzzle: ",  parser2,        solve1star) 
        ] 

type Parser = FilePath -> IO Data
type Solver = Data -> Int

parser :: Parser
parser f = do
        s <- readFile f
        let numpart = transpose $ map (words) (init $ lines s)
            nums = map (map read) numpart
            opspart = words $ last $ lines s
            ops = map read opspart            
        return (zip nums ops)
        
------------------------------------------------------------------
-- This logic doesn't work on 2nd star, because we need the
-- original position of the digits
-----------------------------------------------------------------
parser2 :: Parser -- FilePath->[([Int], Op)]
parser2 f = do
        s <- readFile f
        let numpart = transpose (init $ lines s)
            -- nums :: [[Int]]
            -- fold over the transposed text (right to left, top to down)
            -- spaces (readMaybe return Nothing) is the separator
            nums = foldr
                foldH
                [[]]
                numpart where
                        foldH :: String -> [[Int]] -> [[Int]]
                        foldH nS p@(pA:ps) =
                                case readMaybe nS of
                                        Nothing-> []:p
                                        Just i->(i:pA):ps
            opspart = words $ last $ lines s
            ops = map read opspart            
        return (zip nums ops)



------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------
groupList :: Int -> [a] -> [[a]]
groupList n list = go 1 list [] where
        go _ []     acc = [acc]
        go i (a:as) acc
                | i<n   = go (i+1) as (acc ++ [a])
                | i==n  = (acc++[a]) : go 1 as []

{-
groupList (a1:a2:a3:as) = [a1,a2,a3] : groupList as
groupList (a1:a2:[])    = [[a1,a2]]
groupList (a1:[])       = [[a1]]
groupList ([])          = []
-}



------------------------------------------------------------------
-- Main solvers
-----------------------------------------------------------------

type Problem = ([Int], Op)
type Data = [Problem]

data Op = Plus | Mul

instance Show Op where
        show Plus = "Plus"
        show Mul  = "Mul"

instance Read Op where
        readsPrec _ = readP_to_S parseOp

parseOp :: ReadP Op
parseOp = do
    op <- string "+" +++ string "*"
    case op of
        "+" -> return Plus
        "*" -> return Mul
        _   -> fail "Operation not known!"

runProblem :: Problem -> Int
runProblem (is, Plus) = sum is
runProblem (is, Mul)  = product is

solve1star :: Solver
solve1star = sum . map runProblem

main :: IO ()
main = do
        mapM_
                (\(f, mess, p, m) -> do
                                s<-p f
                                putStrLn $ mess ++
                                                show (m s)
                )
                programMatrix



