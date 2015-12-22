import Control.Monad (liftM)
import Data.List.Split
import Data.List (isInfixOf)

readInputLines :: String ->  IO [String]
readInputLines f = liftM lines (readFile f)

main :: IO ()
main = do 
    ls <- readInputLines "input.txt"
    let sues = map parseLine ls
    let validSues = filter validForAllConditions sues
    print $ length validSues
    print validSues
    return ()

main2 :: IO ()
main2 = do 
    ls <- readInputLines "input.txt"
    let sues = map parseLine ls
    let validSues = filter validForAllConditions2 sues
    print $ length validSues
    print validSues
    return ()

validForAllConditions :: SueInfo -> Bool
validForAllConditions s = all ($ s) [has3Children, has7cats, has2samoyeds, has3pomeranians, has0akitas, has0vizslas, has5goldfish, has3trees, has2cars, has1perfumes]

validForAllConditions2 :: SueInfo -> Bool
validForAllConditions2 s = all ($ s) [has3Children, hasMoreThan7Cats, has2samoyeds, hasLessThan3Pomeranians, has0akitas, has0vizslas, hasLessThan5goldfish, hasMoreThan3trees, has2cars, has1perfumes]

maybeHasX :: (Int -> Int -> Bool) -> Int -> Maybe Int -> Bool
maybeHasX f n (Just i) = f n i
maybeHasX _ _ Nothing  = True

has3Children :: SueInfo -> Bool
has3Children si = maybeHasX (==) 3 $ children si

has7cats :: SueInfo -> Bool
has7cats si = maybeHasX (==) 7 $ cats si

has2samoyeds :: SueInfo -> Bool
has2samoyeds si = maybeHasX (==) 2 $ samoyeds si

has3pomeranians :: SueInfo -> Bool
has3pomeranians si = maybeHasX (==) 3 $ pomeranians si

has0akitas :: SueInfo -> Bool
has0akitas si = maybeHasX (==) 0 $ akitas si

has0vizslas :: SueInfo -> Bool
has0vizslas si = maybeHasX (==) 0 $ vizslas si

has5goldfish :: SueInfo -> Bool
has5goldfish si = maybeHasX (==) 5 $ goldfish si

has3trees :: SueInfo -> Bool
has3trees si = maybeHasX (==) 3 $ trees si

has2cars :: SueInfo -> Bool
has2cars si = maybeHasX (==) 2 $ cars si

has1perfumes :: SueInfo -> Bool
has1perfumes si = maybeHasX (==) 1 $ perfumes si

hasMoreThan3trees :: SueInfo -> Bool
hasMoreThan3trees si = maybeHasX (<) 3 $ trees si

hasMoreThan7Cats :: SueInfo -> Bool
hasMoreThan7Cats si =  maybeHasX (<) 7 $ cats si

hasLessThan3Pomeranians :: SueInfo -> Bool
hasLessThan3Pomeranians si = maybeHasX (>) 3 $ pomeranians si

hasLessThan5goldfish :: SueInfo -> Bool
hasLessThan5goldfish si = maybeHasX (>) 5 $ goldfish si

data SueInfo = SueInfo { name :: String 
                     ,children :: Maybe Int  
                     ,cats :: Maybe Int  
                     ,samoyeds :: Maybe Int 
                     ,pomeranians :: Maybe Int  
                     ,akitas :: Maybe Int  
                     ,vizslas :: Maybe Int  
                     ,goldfish :: Maybe Int  
                     ,trees :: Maybe Int  
                     ,cars :: Maybe Int
                     ,perfumes :: Maybe Int    
                     } deriving (Show)  

parseLine :: String -> SueInfo
parseLine s = SueInfo name children cats samoyeds pomeranians akitas vizslas goldfish trees cars perfumes
    where 
        parts = splitOn "," s
        name = takeWhile (/=':') $ head parts
        firstItem = dropWhile (/=':') $ head parts
        allItems = firstItem:tail parts
        children = hasKey "children" allItems
        cats = hasKey "cats" allItems
        samoyeds = hasKey "samoyeds" allItems
        pomeranians = hasKey "pomeranians" allItems
        akitas = hasKey "akitas" allItems
        vizslas = hasKey "vizslas" allItems
        goldfish = hasKey "goldfish" allItems
        trees = hasKey "trees" allItems
        cars = hasKey "cars" allItems
        perfumes = hasKey "perfumes" allItems


hasKey :: String -> [String] -> Maybe Int
hasKey _ []      = Nothing
hasKey s (s':ss) = if s `isInfixOf` s' then Just value else hasKey s ss
    where
        value = (read . last . words) s' :: Int
