
import Data.List (subsequences, sortBy)
import Data.Function (on)

getInput :: [Int]
getInput = [11,30,47,31,32,36,3,1,5,3,32,36,15,11,46,26,28,1,19,3]

main :: IO ()
main = do 
    let allSubsequences = subsequences getInput
    let valdiSubsequences = filter ((==150) . sum) allSubsequences
    print $ length valdiSubsequences

main2 :: IO ()
main2 = do 
    let allSubsequences = subsequences getInput
    let valdiSubsequences = filter ((==150) . sum) allSubsequences
    let sorted = sortBy (compare `on` length) valdiSubsequences
    print $ length $ filter ((==length (head sorted)) . length) sorted