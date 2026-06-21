module Main where

-- import                  Data.List (transpose, intercalate, unfoldr)
-- import                  Text.ParserCombinators.ReadP
-- import                  Text.Read (readMaybe)
import qualified        Control.Monad.State     as ST
import                  Data.Maybe (mapMaybe)

testFile :: FilePath
testFile = "Day_7_test_data.txt"

puzzleFile :: FilePath
puzzleFile = "Day_7_puzzle_data.txt"

programMatrix :: [(FilePath, String, Parser, Solver)]
programMatrix =
        [
        (testFile,      "1st star - Test: ",    parser,         solve1star)
--      , (puzzleFile,    "1st star - Puzzle: ",  parser,         solve1star) 
--      , (testFile,      "2nd star - Test: ",    parser2,        solve1star) 
--      , (puzzleFile,    "2nd star - Puzzle: ",  parser2,        solve1star) 
        ] 



------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------

changePrev :: (a->Bool) -> a -> [a] -> [a]
changePrev p v xs =
        mapMaybe
        (\case
                (Just curr, Just next)  -> if p next then Just v else Just curr
                --(Nothing,   Just _)     -> Nothing
                (Just curr, Nothing)    -> Just curr
                _                       -> Nothing
        )    
        $ zip (Nothing: map Just xs) (map Just xs++[Nothing])

changeNext :: (a->Bool) -> a -> [a] -> [a]
changeNext p v xs =
        mapMaybe
        (\case
                (Just prev, Just curr)  -> if p prev then Just v else Just curr
                --(Just curr, Nothing)    -> Nothing
                (Nothing,   Just curr)  -> Just curr
                _                       -> Nothing
        )    
        $ zip (Nothing: map Just xs) (map Just xs++[Nothing])

changeAdjacents :: (a->Bool) -> a -> [a] -> [a]
changeAdjacents p v xs = changePrev p v $ changeNext p v xs

------------------------------------------------------------------
-- Main solvers
-----------------------------------------------------------------
type Parser = FilePath -> IO Data
type Solver = Data -> Int

data Space = Empty | Beam | Source | Splitter | ActiveSplitter
        deriving (Eq,Show)

type Manifold = [Space]
type Data = [Manifold]

readSpace :: Char -> Space
readSpace 'S' = Source
readSpace '^' = Splitter
readSpace '|' = Beam
readSpace _   = Empty

instance Semigroup Space where
        Source  <> Empty  = Beam
        Empty   <> Source = Beam
        Beam    <> Beam   = Beam
        Beam    <> Source = Beam
        Source  <> Beam   = Beam
        Splitter <> Beam     = ActiveSplitter
        Beam     <> Splitter = ActiveSplitter
        Empty   <> s      = s
        s       <> Empty  = s


readManifold :: String -> Manifold
readManifold = map readSpace

mergeManifolds :: Manifold -> Manifold -> Manifold
mergeManifolds m1 m2 = map (\(a,b) ->  a <> b) (zip m1 m2)



parser :: Parser
parser f = do
        s <- readFile f
        return $ map readManifold $ lines s




solve1star :: Solver
solve1star = undefined

tachyonPropagation :: Manifold -> ST.State Manifold Int
tachyonPropagation man = do
        s <- ST.get
        return 1
        

main :: IO ()
main = do
        mapM_
                (\(f, mess, p, m) -> do
                                s<-p f
                                putStrLn $ mess ++
                                                show (m s)
                )
                programMatrix
