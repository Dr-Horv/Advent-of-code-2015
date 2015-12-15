{-# LANGUAGE PatternGuards #-}

import Data.List
import Data.List.Split
import Data.Char (isDigit)

main :: IO ()
main = do 
    content <- readFile "input.txt"
    let ls = lines content
    let instructions = map parseInstruction ls
    let g = createGrid instructions
    putStr . show $ countTrue g 
    return ()

type Grid = [[Int]]
type Pos = (Int, Int)
type Instruction = (Pos, Pos, (Int -> Int))

parseInstruction :: String -> Instruction
parseInstruction s  | Just s' <- stripPrefix "turn on " s    = createInstruction s' on
                    | Just s' <- stripPrefix "turn off " s   = createInstruction s' off
                    | Just s' <- stripPrefix "toggle " s     = createInstruction s' toggle
                    | otherwise                              = error $ "Error, invalid instruction: " ++ s

off :: Int -> Int
off 0 = 0
off n = n-1

toggle :: Int -> Int
toggle n = n+2

on :: Int -> Int
on n = n+1

createInstruction :: String -> (Int -> Int) -> Instruction
createInstruction s f = (first_pos, second_pos, f)
    where
        ps = getListOfPositions first_pos second_pos
        (first_pos, second_pos) = getPositions s

createGrid :: [Instruction] -> Grid
createGrid is = chunksOf 1000 $ [ applyRelevantInstructions is (x,y) | x <- [0..999] , y <- [0..999] ]

applyRelevantInstructions :: [Instruction] -> Pos -> Int
applyRelevantInstructions is p = foldl' apply 0 ris 
    where
        ris = filter (\(p1, p2, _) -> positionWithinRect p (p1, p2)) is
        apply :: (Int -> Instruction -> Int)
        apply b (_, _, f) = f b

countTrue :: Grid -> Int
countTrue g = sum $ concat g


getListOfPositions :: Pos -> Pos -> [Pos]
getListOfPositions (x1,y1) (x2, y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]] 

getPositions :: String -> (Pos, Pos)
getPositions s = (extractPos s1, extractPos s2)
    where
        s1 = head ws
        s2 = last ws
        ws = words s

extractPos :: String -> (Int, Int)
extractPos s = (p, p')
    where
        p = (read . twd) s
        p' = (read . reverse . twd . reverse) s
        twd :: [Char] -> [Char]
        twd s = takeWhile isDigit s


positionWithinRect :: Pos -> (Pos, Pos) -> Bool
positionWithinRect (x, y) ((x1, y1), (x2, y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2 

printGrid :: Grid -> IO ()
printGrid g = sequence_ $ stuff
    where
        stuff = map (putStr . nicefyRow) g
        nicefyRow :: [Int] -> String
        nicefyRow ls = (show ls) ++ "\n"