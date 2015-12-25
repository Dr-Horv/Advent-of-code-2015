
import Data.List (permutations, sort, nub)

readInputLines :: String ->  IO [String]
readInputLines f = readFile f >>= (\c -> (return . lines) c) 

main1 :: IO ()
main1 = do 
    content <- readInputLines "input.txt"
    let neigbourValues = map parseLine content
    let peopleNames = nub $ map extractPerson neigbourValues
    let possibleSeatings = permutations peopleNames
    let valuesOfSeatings = map (totalHappines . (happinessOfSeating neigbourValues)) possibleSeatings 
    print $ head $ reverse $ sort valuesOfSeatings

    return ()

main :: IO ()
main = do 
    content <- readInputLines "input.txt"
    let neigbourValues = map parseLine content
    let peopleNames = nub $ map extractPerson neigbourValues
    let extendedNeigbourValues = neigbourValues ++ concatMap (\s -> [("You", s, 0),(s, "You", 0)]) peopleNames
    let extendedPeopleNames = ("You":peopleNames)
    let possibleSeatings = permutations extendedPeopleNames
    let valuesOfSeatings = map (totalHappines . (happinessOfSeating extendedNeigbourValues)) possibleSeatings 
    print $ head $ reverse $ sort valuesOfSeatings

    return ()


type NeighbourValue = (String, String, Int)

totalHappines :: [(String, String, Int)] -> Int
totalHappines ls = foldl (\v (_,_,i) -> i+v) 0 ls

happinessOfSeating :: [NeighbourValue] -> [String] -> [(String, String, Int)]
happinessOfSeating nvs ppl@(p:pps) = [(p, lp, v1), (lp, p, v2)] ++ f ppl nvs 
    where
        lp = last ppl
        v1 = findNeigbourValue p lp nvs
        v2 = findNeigbourValue lp p nvs
        f :: [String] -> [NeighbourValue] -> [(String, String, Int)] 
        f [] _           = [] 
        f (s:[]) _       = [] 
        f (s1:s2:ss) nvs = (s1, s2, findNeigbourValue s1 s2 nvs):(s2, s1, findNeigbourValue s2 s1 nvs):(f (s2:ss) nvs)

extractPerson :: NeighbourValue -> String
extractPerson (s, _, _) = s

findNeigbourValue :: String -> String -> [NeighbourValue] -> Int
findNeigbourValue s1 s2 [] = error $ "Did not find relationship between " ++ s1 ++ " and " ++ s2
findNeigbourValue s1 s2 ((n1, n2, i):nvs) | n1 == s1 && n2 == s2 = i
                                          | otherwise = findNeigbourValue s1 s2 nvs 

parseLine :: String -> NeighbourValue
parseLine s = (name, targetName, modifier * value)
    where
        parts = words s
        name = head parts
        modifier = if parts !! 2 == "gain" then 1 else -1
        value = read (parts !! 3) :: Int 
        lastPart = last parts 
        targetName = take (length lastPart - 1) lastPart 