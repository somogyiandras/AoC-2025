{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import AoC.Framework

import                  Data.Set (Set, singleton, member, insert,
                                  fromList, toAscList, delete,
                                  unions, elems, size, empty)
import                  Data.List.HT (partition)
import                  Data.List (sortBy)
import                  Data.Char (isDigit)
import                  Text.ParserCombinators.ReadP
import                  Data.Function (on)
import qualified        Control.Monad.State as ST


--
-----------------------------------------------------------------
-- functions to run the solver
-----------------------------------------------------------------
programMatrix :: [Task Config Data]
programMatrix =
    [ Task testConfig   "1st star - Test: "   parsing solve1stStar
    , Task puzzleConfig "1st star - Puzzle: " parsing solve1stStar
    , Task testConfig2 "2nd star - Test: "    parsing solve2ndStar
--    , Task puzzleConfig2 "2nd star - Puzzle: " parsing solve1stStar
    ]

------------------------------------------------------------------
-- Main data structures, functions
------------------------------------------------------------------

data Config = Config
    { fileName  :: FilePath
    , pairCount :: Int
    }

data Pos = Pos { x, y, z :: Int } deriving (Eq)
type Data = [Pos]

instance Show Pos where
    show (Pos {..}) = "(-" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "-)"

instance Read Pos where
    readsPrec _ = readP_to_S parsePos

parsePos :: ReadP Pos
parsePos = do
    x <- read <$> munch1 isDigit
    _ <- char ','
    y <- read <$> munch1 isDigit
    _ <- char ','
    z <- read <$> munch1 isDigit
    return Pos {x=x, y=y, z=z}

instance Ord Pos where
    compare (Pos x1 y1 z1) (Pos x2 y2 z2) = compare (x1, y1, z1) (x2, y2, z2)

distPos :: Pos -> Pos -> Double
distPos p1 p2 = sqrt (fromIntegral (dx*dx + dy*dy + dz*dz))
  where
    dx = x p1 - x p2
    dy = y p1 - y p2
    dz = z p1 - z p2

origo :: Pos
origo = Pos {x=0, y=0, z=0}

lengthPos :: Pos -> Double
lengthPos = distPos origo

newtype Pair a = Pair (a, a) deriving Show

instance Eq a =>Eq (Pair a) where
    (Pair (x1, y1)) == (Pair (x2, y2)) = (x1==x2 && y1==y2) || (x1==y2 && y1==x2)

instance Ord (Pair Pos) where
    compare = compare `on` distPair

distPair :: Pair Pos -> Double
distPair (Pair (p1, p2)) = distPos p1 p2

closestPairs :: Int -> [Pos] -> [Pair Pos]
closestPairs n = take n . allPairs

allPairs :: [Pos] -> [Pair Pos]
allPairs ps = 
        toAscList $
        fromList [Pair (p1, p2) | p1<-ps, p2<-ps, p1/=p2, p1<p2]

type Circuit = Set Pos

initCircuits :: [Pos] -> [Circuit]
initCircuits = map singleton

-- chatGPT suggestion
--
mergePair :: Pair Pos -> [Circuit] -> [Circuit]
mergePair (Pair (p1, p2)) circuits =
        let (contains, others) = partition (\ c -> p1 `member` c || p2 `member` c) circuits
            newCircuit = unions (fromList [p1, p2] : contains)
        in newCircuit : others

------------------------------------------------------------------

mergePair2 :: Pair Pos -> (Maybe (Pair Pos), [Circuit]) -> (Maybe (Pair Pos), [Circuit])
mergePair2 (Pair (p1, p2)) (_, circuits) =
        let (contains, others) = partition (\ c -> p1 `member` c || p2 `member` c) circuits
            newCircuit = unions (fromList [p1, p2] : contains)
        in if null others
            then (Just $ Pair (p1, p2), [newCircuit])
            else (Nothing, newCircuit:others)


parsing :: Config -> IO Data
parsing  c = do
        s <- readFile $ fileName c
        return $ map read $ lines s



solve1stStar :: Config -> Data -> Int
solve1stStar c d =
        let cl = closestPairs (pairCount c)  d
            initialCircuits = initCircuits d
            endCircuits = foldr mergePair initialCircuits cl
        in product $ map size $ take 3 $ sortBy (flip compare `on` size) endCircuits


{-
solve1stStar :: Config -> Data -> Int
solve1stStar c d =
        let cl = closestPairs (pairCount c)  d
            startCircuits = cl ++ initCircuits d
            endCircuits = fixPass startCircuits
        in product $ map size $ take 3 $ sortBy (flip compare `on` size) endCircuits
-}

solve2ndStar :: Config -> Data -> Int
solve2ndStar c d =
        let cl = allPairs d
            initialCircuits = initCircuits d
            (pair, endCircuits) = foldr mergePair2 (Nothing, initialCircuits) cl
        in case pair of 
            Nothing -> -1
            Just (Pair (p1, p2)) -> x p1 * x p2


-----------------------------------------------------------------------------------
-- Main part: parameters for the problems
--

testFile :: FilePath
testFile = "Day_8_test_data.txt"

testConfig :: Config
testConfig = Config testFile 10

puzzleFile :: FilePath
puzzleFile = "Day_8_puzzle_data.txt"

puzzleConfig :: Config
puzzleConfig = Config {fileName = puzzleFile, pairCount = 1000}

testConfig2 :: Config
testConfig2 = Config testFile 10

main :: IO ()
main = runAoC programMatrix
