import Data.List.Split
import Data.List (nub, sort, permutations)

readInputLines :: String ->  IO [String]
readInputLines f = readFile f >>= (return . lines) 

main :: IO ()
main = do 
    ls <- readInputLines "input.txt"
    let paths = map readPath ls
    let cityNames = cities paths
    let possibleRoutes = filter (isValidRoute paths) $ permutations cityNames
    let distancesOfRoutes = map (totalDistanceOfPath paths) possibleRoutes
    print $ head $ reverse $ sort distancesOfRoutes

    return ()

totalDistanceOfPath :: [Path] -> [String] -> Int
totalDistanceOfPath _ (x:[]) = 0
totalDistanceOfPath _ []     = 0
totalDistanceOfPath ps (s1:s2:ss) = distanceBetween ps s1 s2 + totalDistanceOfPath ps (s2:ss) 

distanceBetween :: [Path] -> String -> String -> Int
distanceBetween [] s1 s2 = error $ "Could not find distance between " ++ s1 ++ " and " ++ s2
distanceBetween ((c1,c2,i):cs) s1 s2 = if c1 == s1 && c2 == s2 || c1 == s2 && c2 == s1 then i else distanceBetween cs s1 s2

cities :: [Path] -> [String]
cities ps = nub $ concatMap (\(s1,s2,_) -> [s1,s2]) ps

isValidRoute :: [Path] -> [String] -> Bool
isValidRoute _ (s:[])      = True
isValidRoute _          [] = True
isValidRoute ps (s1:s2:ss) = isConnected ps s1 s2 && isValidRoute ps (s2:ss)

isConnected :: [Path] -> String -> String -> Bool
isConnected []     _ _            = False
isConnected ((c1,c2,_):cs) s1 s2  = c1 == s1 && c2 == s2 || c1 == s2 && c2 == s1 || isConnected cs s1 s2


type Path = (String, String, Int)

readPath :: String -> Path
readPath s = (c1, c2, d)
    where 
        parts = words s
        c1 = head parts
        c2 = parts !! 2
        ds = last parts
        d = read ds :: Int 
