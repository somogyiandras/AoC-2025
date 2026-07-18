module Main where

import qualified        Data.Map.Lazy           as Map
import                  Data.Maybe (mapMaybe, fromJust)
import qualified        Control.Monad.State     as ST

type Cell = (Int, Int)

type IntGrid  = Map.Map Cell Int
type BoolGrid = Map.Map Cell Bool

readBoolGrid :: String -> BoolGrid
readBoolGrid s = Map.fromList [((i,fst c), snd c == '@') | (i,l) <- zip [0..] $ lines s, c <- zip [0..] l ]

cleanBoolGrid :: BoolGrid -> BoolGrid
cleanBoolGrid = Map.filter id

getAdjacentValues :: Cell -> BoolGrid -> [Bool]
-- to generalize the Grid class (Grid a), then Cell->Grid a->[a]
getAdjacentValues (i,j) grid =
        mapMaybe (\a -> Map.lookup a grid)
        [ (i-1,j-1)
        , (i-1,j  )
        , (i-1,j+1)
        , (i  ,j+1)
        , (i+1,j+1)
        , (i+1,j  )
        , (i+1,j-1)
        , (i  ,j-1)
        ]

count :: (a -> Bool) -> [a] -> Int
count f = length . (filter f)

calculateRolls :: String -> Map.Map Cell Int
calculateRolls s = let
        grid = readBoolGrid s
               -- read the data to the BoolGrid (True if cell containes a roll)
        in Map.mapWithKey
            (\k _ -> if fromJust $ Map.lookup k grid
            -- only for the not empty cells
                    then count id $ getAdjacentValues k grid
                    -- count the number of adjacent rolls
                    else maxBound)
            grid



forklifting :: ST.State BoolGrid Int
forklifting = do
        st <- ST.get
        let gridAdjacentRolls =
             Map.mapWithKey
                (\k _ -> if fromJust $ Map.lookup k st
                        then count id $ getAdjacentValues k st
                        else maxBound)
                st
            newst = Map.mapWithKey
                (\k v -> if k `elem` (Map.keys $ Map.filter (<4) gridAdjacentRolls)
                        then False
                        else v)
                st
        ST.put newst
        return $ count (<4) $ Map.elems gridAdjacentRolls




main :: IO ()
main = do
        putStr "First star - Test data - Answer: "
        grid_txt  <- readFile "Day_4_test_data.txt"
        let grid_adjacents = calculateRolls grid_txt
        putStrLn $ show $ count (<4) $ Map.elems grid_adjacents

        putStr "First star - Puzzle data - Answer: "
        grid_txt  <- readFile "Day_4_puzzle_data.txt"
        let grid_adjacents = calculateRolls grid_txt
        putStrLn $ show $ count (<4) $ Map.elems grid_adjacents

        putStr "Second star - Test data - Answer: "
        grid_txt  <- readFile "Day_4_test_data.txt"
        let grid = readBoolGrid grid_txt
        putStrLn $ show $ sum $ takeWhile (>0) $ fst $ ST.runState (sequence $ repeat forklifting ) grid

        putStr "Second star - Puzzle data - Answer: "
        grid_txt  <- readFile "Day_4_puzzle_data.txt"
        let grid = readBoolGrid grid_txt
        putStrLn $ show $ sum $ takeWhile (>0) $ fst $ ST.runState (sequence $ repeat forklifting ) grid
