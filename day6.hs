{-# LANGUAGE PatternGuards #-}

import Data.List
import Data.Char (isDigit)

main :: IO ()
main = do 
    content <- readFile "input.txt"
    let ls = lines content
    let grid = getXbyYBoard 1000 1000
    let g = foldl' doInstruction grid ls
    putStr . show $ countTrue g 
    return ()


doInstruction :: Grid -> String -> Grid
doInstruction g s = updateState s g 

countTrue :: Grid -> Int
countTrue g = length $ filter id $ concat g

getXbyYBoard :: Int -> Int -> [[Bool]]
getXbyYBoard x y = replicate x $ replicate y False

type Grid = [[Bool]]
type Pos = (Int, Int)

updateState :: String -> Grid -> Grid
updateState s g | Just s' <- stripPrefix "turn on " s    = handleTurnOn s' g
                | Just s' <- stripPrefix "turn off " s   = handleTurnOff s' g
                | Just s' <- stripPrefix "toggle " s     = handleToggle s' g
                | otherwise                              = error "s"

handleTurnOff :: String -> Grid -> Grid
handleTurnOff s g = handle off s g 
    where 
        off :: Bool -> Bool
        off _ = False

handleToggle :: String -> Grid -> Grid
handleToggle s g = handle toggle s g
    where
        toggle :: Bool -> Bool
        toggle b = not b

handleTurnOn :: String -> Grid -> Grid
handleTurnOn s g = handle on s g
    where 
        on :: Bool -> Bool
        on _ = True

handle :: (Bool -> Bool) -> String -> Grid -> Grid
handle f s g = aft f g ps --foldl' (applyFunOnPos f) g ps
    where 
        ps = getListOfPositions first_pos second_pos
        (first_pos, second_pos) = getPositions s

aft :: (Bool -> Bool) -> Grid -> [Pos] -> Grid
aft _ g [] = g
aft f g (p:ps) = recCall `seq` aft f recCall ps 
    where
        recCall = (applyFunOnPos f g p)


nothingDo :: Pos -> Pos -> Pos 
nothingDo _ _ = (0, 0)

getPos :: Pos
getPos = (0, 0)

getListOfPositions :: Pos -> Pos -> [Pos]
getListOfPositions (x1,y1) (x2, y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]] 

applyFunOnPos :: (Bool -> Bool) -> Grid -> Pos -> Grid
applyFunOnPos f g p@(x,y) = update g p v 
    where
        v = f ((g !! x) !! y)

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

(!!=) :: [a] -> (Int,a) -> [a]
(x:xs) !!= (0, a)     = a:xs
(x:xs) !!= (n, a)     = x : (!!=) xs (n-1, a)
[]     !!=  _         = error "Index out of bounds"

update :: Grid -> Pos -> Bool -> Grid 
update g (x,y) a = g !!= (x,row)
    where 
        row = g !! x !!= (y, a) 

printGrid :: Grid -> IO ()
printGrid g = sequence_ $ stuff
    where
        stuff = map (putStr . nicefyRow) g
        nicefyRow :: [Bool] -> String
        nicefyRow ls = (show ls) ++ "\n"