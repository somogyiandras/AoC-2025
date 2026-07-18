module Main where

import                  Data.List (permutations)
-- import                  Text.ParserCombinators.ReadP
-- import                  Text.Read (readMaybe)
-- import qualified        Control.Monad.State     as ST
-- import                  Data.Maybe (mapMaybe)

-----------------------------------------------------------------
-- functions to run the solver
-----------------------------------------------------------------
testFile :: FilePath
testFile = "Day_7_test_data.txt"

puzzleFile :: FilePath
puzzleFile = "Day_7_puzzle_data.txt"

type Parser = FilePath -> IO Data
type Solver = Data -> Int

programMatrix :: [(FilePath, String, Parser, Solver)]
programMatrix =
        [
        (testFile,      "1st star - Test: ",    parser,         solve1star)
      , (puzzleFile,    "1st star - Puzzle: ",  parser,         solve1star) 
      , (testFile,      "2nd star - Test: ",    parser,         solve2star) 
      , (puzzleFile,    "2nd star - Puzzle: ",  parser,         solve2star) 
        ] 



------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count f = length . (filter f)

data Q = L | R
        deriving (Eq,Show)

type QS = [Q]

------------------------------------------------------------------
-- Main data structures, functions
-----------------------------------------------------------------
data Pos = Empty | Beam | Source | Splitter | ActiveSplitter
        deriving (Eq, Show)

printPos :: Pos -> Char
printPos = \case
            Empty           -> '.'
            Source          -> 'S'
            Splitter        -> '^'
            ActiveSplitter  -> '$'
            Beam            -> '|'

{-
instance Show Position where
        show = \case
                Empty           -> "."
                Source          -> "S"
                Splitter        -> "^"
                ActiveSplitter  -> "$"
                Beam            -> "|"
-}


readPos :: Char -> Pos
readPos 'S' = Source
readPos '^' = Splitter
readPos '|' = Beam
readPos _   = Empty

(.^.) :: Pos-> Pos-> Pos
(.^.)    = \cases
    Source          Empty           -> Beam
    Empty           Source          -> Source
    Empty           Splitter        -> Splitter
    Splitter        Empty           -> Empty
    Empty           s               -> s
    s               Empty           -> s
    Beam            Beam            -> Beam
    Beam            Source          -> Beam
    Source          Beam            -> Beam
    Splitter        Beam            -> ActiveSplitter
    Beam            Splitter        -> ActiveSplitter
    Source          Splitter        -> ActiveSplitter
    Splitter        Source          -> ActiveSplitter    
    Source          Source          -> Beam
    Splitter        Splitter        -> Splitter
    ActiveSplitter  _               -> ActiveSplitter
    _               ActiveSplitter  -> ActiveSplitter
            
type Line = [Pos]

readLine :: String -> Line
readLine = (map readPos)

printLine :: Line -> String
printLine = map printPos

mergeLines :: Line -> Line -> Line
mergeLines l1 l2 = map (\(a,b) ->  a .^. b) (zip l1 l2)

-- ActiveSplitter creates two beams in the adjacent positions
-- deactivate splitter
activateSplitters :: Line -> Line
activateSplitters l =
        zipWith3
        zipper
        (Empty:l)
        (l)
        (tail l ++ [Empty])
        where
                zipper :: Pos->Pos->Pos->Pos
                zipper _                Beam    _               = Beam
                zipper _                _       ActiveSplitter  = Beam
                zipper ActiveSplitter   _       _               = Beam
                zipper _                ActiveSplitter _        = Splitter
                zipper _                pos     _               = pos

activateSplitters2 :: (QS,Line) -> [(QS,Line)]
activateSplitters2 (qs, l)
        | (count (ActiveSplitter==) l) == 0 = [(qs, l)]
        | otherwise                         =
                [(qsL, lL), (qsR, lR)] where
                lR  = zipWith zippR (Empty:l) l
                lL  = zipWith zippL l         (tail l ++ [Empty])
                qsL = qs ++ [L]
                qsR = qs ++ [R]
                zippR :: Pos->Pos->Pos
                zippR _                Beam             = Beam
                zippR ActiveSplitter   _                = Beam
                zippR _                ActiveSplitter   = Splitter
                zippR _                pos              = pos
                zippL :: Pos->Pos->Pos
                zippL Beam             _                = Beam
                zippL ActiveSplitter   _                = Splitter
                zippL _                ActiveSplitter   = Beam    
                zippL pos              _                = pos
        
type Manifold = [Line]
type Data = Manifold

renderManifold :: Manifold -> String
renderManifold = unlines . map printLine

propagateD :: Manifold -> (Int, Manifold)
propagateD m =
        let
                (sumSplits, man) = foldl
                                propagate
                                (0, [fst $ unzip $ zip (repeat Empty) (head m)])
                                m 
        in (sumSplits, tail man)

propagate :: (Int, Manifold) -> Line -> (Int, Manifold)
propagate (splitsSum, m) l = 
        let
                nextLine = mergeLines (last m) l
                splits   = count (ActiveSplitter==) nextLine
        in (splitsSum + splits, m ++ [activateSplitters $ nextLine])


propagateD2 :: Manifold -> [(QS, Manifold)]
propagateD2 m = foldl
                propagate2
                []
                --([]::QS, [fst $ unzip $ zip (repeat Empty) (head m)])
                m

propagate2 :: [(QS, Manifold)] -> Line -> [(QS, Manifold)]
propagate2 [] l = [([], [l])]
propagate2 ms l = 
        foldl
        (go l)
        []
        ms

go :: Line -> [(QS, Manifold)] -> (QS, Manifold) -> [(QS, Manifold)]
go l acc (qs, currManifold) =
        let
                currLine = last currManifold
                nextLine = mergeLines currLine l
                splitTime = activateSplitters2 (qs, nextLine)
                nextManifolds =
                        map
                          (\ (qs',l') -> (qs, currManifold)<> (qs', [l']))
                          splitTime
        in  acc ++ nextManifolds


parser :: Parser
parser f = do
        s <- readFile f
        return $ map readLine $ lines s

solve1star :: Solver
solve1star d = fst $ propagateD d

solve2star :: Solver
solve2star d = length $ propagateD2 d

main :: IO ()
main = do
        mapM_
                (\(f, mess, p, m) -> do
                                s<-p f
                                putStrLn $ mess ++
                                                show (m s)
                )
                programMatrix
