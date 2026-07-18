module Main where

import                  Data.Char (isDigit)
import                  Text.ParserCombinators.ReadP
import qualified        Control.Monad.State as ST

magicnumber :: Int
magicnumber = 100

star1_test_data :: String
star1_test_data = """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"""

star1_test_start :: (Int, Int)
star1_test_start = (50, 0)

star1_data_name :: FilePath
star1_data_name = "input.txt"

data Rot =
        L Int
      | R Int
        deriving (Eq, Ord)

instance Show Rot where
        show (R i) = 'R' : show i
        show (L i) = 'L' : show i

instance Read Rot where
        readsPrec _ = readP_to_S parseRot

parseRot :: ReadP Rot
parseRot = do
    rot <- char 'R' +++ char 'L'
    num <- fmap read $ munch1 isDigit
    case rot of
        'R' -> return (R num)
        'L' -> return (L num)

parseRots :: String -> [Rot]
parseRots = map read . lines

runRot ::  Rot -> ST.State (Int, Int) ()
runRot command = do
        (pos, zeros) <- ST.get
        let     newpos = ( pos + case command of
                                        (R r) -> r
                                        (L r) -> -r
                         ) `mod` magicnumber
                newzeros = zeros + if newpos==0 then 1 else 0
        ST.put (newpos, newzeros)

runRots :: (Int, Int) -> [Rot] -> (Int, Int)
runRots = foldl
        (\ s r -> (ST.execState $ runRot r) s)


readData :: FilePath -> IO [Rot]
readData fname = do
        s <- readFile fname
        return $ parseRots s

runRot2 ::  Rot -> ST.State (Int, Int) ()
runRot2 (R r) = do
        (pos, zeros) <- ST.get
        let     pos' = pos + r
                newpos = pos' `mod` magicnumber
                newzeros = zeros + abs (pos' `div` magicnumber)
        ST.put (newpos, newzeros)
runRot2 (L r) = do
        (pos, zeros) <- ST.get
        let     pos' = (if pos==0 then 100 else pos) - r
                newpos = pos' `mod` magicnumber
                newzeros = zeros + abs (pos' `div` magicnumber) +
                                if newpos == 0 then 1 else 0
        ST.put (newpos, newzeros)

runRots2 :: (Int, Int) -> [Rot] -> (Int, Int)
runRots2 = foldl
        (\ s r -> (ST.execState $ runRot2 r) s)

runRots2' :: (Int, Int) -> [Rot] -> [(Int, Int)]
runRots2' = scanl
        (\ s r -> (ST.execState $ runRot2 r) s)

main :: IO ()
main = do
        putStrLn "First star"
        putStrLn "\nTest data"
        
        let result = runRots star1_test_start (parseRots star1_test_data)
        putStrLn $ show result
        putStrLn $ show $ 3 == snd result

        putStrLn "\nReal data"
        readData star1_data_name >>= ( putStrLn . show . runRots star1_test_start)

        putStrLn "\n\nSecond star"

        putStrLn "\nTest data"
        
        let result = runRots2 star1_test_start (parseRots star1_test_data)
        putStrLn $ show result
        putStrLn $ show $ 6 == snd result

        putStrLn "\nReal data"
        datastring <- readData star1_data_name
        putStrLn $ show $ runRots2 star1_test_start datastring

