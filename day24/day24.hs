
import Data.List (subsequences, sortBy)

getInput :: [Int] 
getInput = [1,2,3,5,7,13,17,19,23,29,31,37,41,43,53,59,61,67,71,73,79,83,89,
            97,101,103,107,109,113]

main1 :: IO ()
main1 = do 
    let input = getInput
    let totalSum = sum input
    let bagWeight = totalSum `div` 3 :: Int
    print bagWeight
    let allSubsequences = subsequences input
    let valdiSubsequences = filter (isValidWeight bagWeight) allSubsequences
    let sorted = sortBy sortF valdiSubsequences
    print $ take 5 sorted
    print $ product $ head sorted

    return ()

main :: IO ()
main = do 
    let input = getInput
    let totalSum = sum input
    let bagWeight = totalSum `div` 4 :: Int
    print bagWeight
    let allSubsequences = subsequences input
    let valdiSubsequences = filter (isValidWeight bagWeight) allSubsequences
    let sorted = sortBy sortF valdiSubsequences
    print $ take 5 sorted
    print $ product $ head sorted

    return ()

isValidWeight :: Int -> [Int] -> Bool
isValidWeight w is = sum is == w

sortF :: [Int] -> [Int] -> Ordering
sortF is is' | l1 < l2 = LT
             | l1 > l2 = GT 
             | l1 == l2 = quantumEntanglementSort is is'
    where
        l1 = length is
        l2 = length is'

quantumEntanglementSort :: [Int] -> [Int] -> Ordering
quantumEntanglementSort is is'  | p1 < p2   = LT
                                | p1 > p2   = GT 
                                | p1 == p2  = EQ
    where
        p1 = product is
        p2 = product is'