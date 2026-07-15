{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import                  Data.Set (Set, singleton, member, 
                                  fromList, toAscList, 
                                  unions, elems, size)
import                  Data.List.HT (partition)
import                  Data.List (sortBy)
import                  Data.Char (isDigit)
import                  Text.ParserCombinators.ReadP
import                  Data.Function (on)
import                  Data.Maybe (fromJust)


--
-----------------------------------------------------------------
-- functions to run the solver
-----------------------------------------------------------------
testFile :: FilePath
testFile = "Day_8_test_data.txt"

puzzleFile :: FilePath
puzzleFile = "Day_8_puzzle_data.txt"

type Parser = FilePath -> IO Data
type Solver = Data -> Int

programMatrix :: [(FilePath, String, Parser, Solver)]
programMatrix =
        [
        (testFile,      "1st star - Test: ",    parser,         solve1star)
--      , (puzzleFile,    "1st star - Puzzle: ",  parser,         solve1star)
--      , (testFile,      "2nd star - Test: ",    parser,         solve2star)
--      , (puzzleFile,    "2nd star - Puzzle: ",  parser,         solve2star)
        ]

{-
test :: IO ()
test = do
        d <- parser testFile
        let c   = initCircuits d
            cl  = closestPairs 10 d
            res = foldCircuits cl c
        putStrLn $ show c
        putStrLn $ show cl
        putStrLn $ show res
        putStrLn $ show $ length res
-}

------------------------------------------------------------------
-- Main data structures, functions
------------------------------------------------------------------
data Pos = Pos { x, y, z :: Int } deriving (Eq)
type Data = [Pos]

instance Show Pos where
    show (Pos {..}) = "(-" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "-)"

instance Read Pos where
    readsPrec _ = readP_to_S parsePos

parsePos :: ReadP Pos
parsePos = do
    x <- fmap read $ munch1 isDigit
    _ <- char ','
    y <- fmap read $ munch1 isDigit
    _ <- char ','
    z <- fmap read $ munch1 isDigit
    return Pos {x=x, y=y, z=z}

instance Ord Pos where
    compare (Pos x1 y1 z1) (Pos x2 y2 z2) = compare (x1, y1, z1) (x2, y2, z2)

distPos :: Pos -> Pos -> Double
distPos p1 p2 = sqrt (fromIntegral ((x p1 - x p2)^2
                                +(y p1 - y p2)^2
                                +(z p1 - z p2)^2))
origo :: Pos
origo = Pos {x=0, y=0, z=0}

lengthPos :: Pos -> Double
lengthPos = distPos origo

data Pair a = Pair (a, a) deriving Show

instance Eq a =>Eq (Pair a) where
    (Pair (x1, y1)) == (Pair (x2, y2)) = (x1==x2 && y1==y2) || (x1==y2 && y1==x2)

instance Ord (Pair Pos) where
    compare = compare `on` distPair

distPair :: Pair Pos -> Double
distPair (Pair (p1, p2)) = distPos p1 p2

makePairSet :: Ord a => Pair a -> Set a
makePairSet (Pair (a,b)) = fromList [a,b]

closestPairs :: Int -> [Pos] -> [Set Pos]
closestPairs n ps = 
        map makePairSet $
        take n $
        toAscList $
        fromList [Pair (p1, p2) | p1<-ps, p2<-ps, p1/=p2, p1<p2]

type Circuit = Set Pos

initCircuits :: [Pos] -> [Circuit]
initCircuits = map singleton

connectSet :: Ord a => Set a -> [Set a] -> Maybe [Set a]
connectSet s ss =
        let (contains, others) = partition 
                                (\ set -> let as = elems s
                                              in any ( `member` set) as)
                                 ss
            in if not $ null contains
                then Just $ (unions contains) : others
                else Nothing

pass :: Ord a => [Set a] -> Maybe [Set a]
pass []  = Nothing
pass (s:ss) = case connectSet s ss of
            Just newSets -> Just newSets
            Nothing      -> case pass ss of
                            Nothing -> Nothing
                            Just zs -> Just (s:zs)

fixPass :: Ord a => [Set a] -> [Set a]
fixPass ss = case pass ss of
            Nothing -> ss
            Just ss' ->  fixPass ss'

parser :: FilePath -> IO Data
parser f = do
        s <- readFile f
        return $ map read $ lines s

solve1star :: Solver
solve1star d =
        let cl = closestPairs 10 d
            startCircuits = cl ++ initCircuits d
            endCircuits = fixPass startCircuits
        in product $ map size $ take 3 $ sortBy (compare `on` size) endCircuits

main :: IO ()
main = do
        mapM_
            (\(f, mess, p, m) -> do
                            s<-p f
                            putStrLn $ mess ++
                                            show (m s)
            )
            programMatrix
