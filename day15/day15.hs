
import Data.Maybe
import Data.List (nub, sort, permutations)

readInputLines :: String ->  IO [String]
readInputLines f = readFile f >>= (return . lines) 

type Ingredient = (String, Int, Int, Int, Int, Int)

main1 :: IO ()
main1 = do 
    ls <- readInputLines "input.txt"
    let ingredients = map parseLine ls
    let scores = map (fst . calculateScoreOfCombination ingredients) getCombinations
    print $ maximum scores 

main :: IO ()
main = do 
    ls <- readInputLines "input.txt"
    let ingredients = map parseLine ls
    let scores = map fst $ filter (\(_, cal) -> cal == 500) $ map (calculateScoreOfCombination ingredients) getCombinations
    print $ maximum scores 

getCombinations :: [[Int]]
getCombinations = [[x,y, z, q] | x <- [0..100], y <- [0..100], z <- [0..100], q <- [0..100], x+y+z+q==100]

calculateScoreOfCombination :: [Ingredient] -> [Int] -> (Int, Int)
calculateScoreOfCombination ins is = (product scoreModifiers, cal)
    where
        scoreModifiers = map (\i -> if i < 0 then 0 else i) [cap, dur, flav, tex]
        (cap, dur, flav, tex, cal) = foldl f (0,0,0,0,0) $ zip is ins
        f :: (Int, Int, Int, Int, Int) -> (Int, Ingredient) -> (Int, Int, Int, Int, Int)
        f (cap, dur, flav, tex, cal) (i, (_, cap', dur', flav', tex', cal')) = (cap + cap'*i, dur+dur'*i, flav+flav'*i, tex+tex'*i, cal+cal'*i)

parseLine :: String -> Ingredient
parseLine s = (name, capacity, durability, flavor, texture, calories)
    where
        parts = words s
        first = head parts
        name = skipLastChar first
        capacity = read (skipLastChar (parts !! 2))
        durability = read (skipLastChar (parts !! 4))
        flavor = read (skipLastChar (parts !! 6))
        texture = read (skipLastChar (parts !! 8))
        calories = read (parts !! 10)
        skipLastChar :: String -> String
        skipLastChar s = take (length s - 1) s 