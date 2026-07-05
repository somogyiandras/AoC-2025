module Main where

-- import                  Data.List (permutations)
-- import                  Text.ParserCombinators.ReadP
-- import                  Text.Read (readMaybe)
import                  Data.Map (fromList, toList, findMax, keys, Map, adjust)
import qualified        Data.Map  as Map (filter, lookup, insert)
import                  Data.Set (Set, member, insert, empty)
import                  Data.Maybe (isNothing)
import qualified        Control.Monad.State as ST


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
------------------------------------------------------------------
{-
count :: (a -> Bool) -> [a] -> Int
count f = length . (filter f)
-}
------------------------------------------------------------------
-- Main data structures, functions
------------------------------------------------------------------
type Pos = (Int, Int)
type Manifold = Map (Int, Int) Char
type Data = Manifold
type Line = String

(.+.) :: Pos -> Pos -> Pos
(x, y) .+. (k, l) = (x+k, y+l)

renderManifold :: Manifold -> IO ()
renderManifold m = mapM_ putStrLn [[c | ((x,_),c) <- toList m, x==i ]|i<-[0..fst $ fst $ findMax m]]

headM :: Manifold -> Line
headM m = [c | ((x,_),c) <- toList m, x==0 ] 

sliceM :: Int -> Manifold -> Manifold
sliceM n m = fromList $ [((x,y),c) | ((x,y),c) <- toList m, i<-[0..n], x==i]

startPos :: Manifold -> [Pos]
startPos = keys . Map.filter (=='S')

addBeam :: Pos -> Manifold -> Manifold
addBeam = adjust (const '|')

splitBeam :: Pos -> Manifold -> Manifold
splitBeam pos m =
        let
            m1  = adjust (const '|') (pos .+. (0,1))    m
            m2  = adjust (const '|') (pos .+. (0,(-1))) m1
            m3  = adjust (const '|') (pos .+. (1,1))    m2
        in        adjust (const '|') (pos .+. (1,(-1))) m3
             
thread :: Pos -> Int -> Manifold -> Set Pos -> (Set Pos, Manifold, Int)
thread pos splits m seen
        | member pos seen = (seen, m, splits)
        | Just 'S' <- Map.lookup pos m = thread (pos .+. (2,0)) splits nextM nextSeen
        | Just '.' <- Map.lookup pos m = thread (pos .+. (2,0)) splits nextM nextSeen
        | Just '^' <- Map.lookup pos m =
            let
                 (leftSeen, leftMan, leftSplits) = thread (pos .+. (2,(-1))) (splits+1) nextMS nextSeenSplit
            in thread (pos .+. (2,1)) (leftSplits) leftMan leftSeen
        | Nothing  <- Map.lookup pos m = (seen, m, splits) 

        where
            nextSeen =  insert pos $ insert (pos .+. (1,0)) seen
            nextM = addBeam (pos .+. (1,0)) $ addBeam pos m
            nextSeenSplit = insert pos $ insert (pos .+. (1,1))
                          $ insert (pos .+. (1,(-1)))
                          $ insert (pos .+. (0,1))
                          $ insert (pos .+. (0,(-1))) seen
            nextMS = splitBeam pos m


parser :: Parser
parser f = do
        s <- readFile f
        return $ fromList [((x,y),c) | (x,line) <- zip [0..] $ lines s, (y,c) <- zip [0..] line]

solve1star :: Solver
solve1star d = case thread (head $ startPos d) 0 d empty of
        (_, _, splits) -> splits

thread2 :: Pos -> Manifold ->  ST.State (Map Pos Int) (Manifold,  Int)
thread2 pos m = do
        memoise <- ST.get
        case Map.lookup pos memoise of
            Just val -> return (m, val)
            Nothing  -> do
                (man, val) <- case currentPos of
                    Just 'S' -> thread2 (pos .+. (2,0)) nextM
                    Just '.' -> thread2 (pos .+. (2,0)) nextM
                    Just '^' -> do 
                        (leftMan, leftWorlds)   <- thread2 (pos .+. (2,(-1))) nextMS
                        (rightMan, rightWorlds) <- thread2 (pos .+. (2,1))  nextMS
                        return (rightMan, leftWorlds + rightWorlds)
                    Nothing -> return (m, 1)
                    Just '|'-> return (m, 1)
                ST.modify (Map.insert pos val)
                return (man, val)

        where
            currentPos = Map.lookup pos m
            nextM = addBeam (pos .+. (1,0)) $ addBeam pos m
            nextMS = splitBeam pos m

solve2star :: Solver
solve2star d = snd $ ST.evalState (thread2 (head $ startPos d) d) (fromList [])


main :: IO ()
main = do
        mapM_
            (\(f, mess, p, m) -> do
                            s<-p f
                            putStrLn $ mess ++
                                            show (m s)
            )
            programMatrix
