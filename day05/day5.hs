
import Data.List (isInfixOf)

main :: IO ()
main = do 
    content <- readFile "input.txt"
    let ls = lines content
    let count = length $ filter id $ testStrings' ls
    putStr (show count)


testStrings :: [String] -> [Bool]
testStrings (s:ss)  = (and $ map ($ s) [(containsXVowels 3), doesNotContainNaughtyStrings, containsRepeatedLetter]):testStrings ss
testStrings []      = []

testStrings' :: [String] -> [Bool]
testStrings' (s:ss)  = (and $ map ($ s) [containsPairOfLettersTwice, containsRepeatedLetterWithPad]):testStrings' ss
testStrings' []      = []

containsXVowels :: Int -> String -> Bool
containsXVowels i s = (length $ filter id $ map (`elem` vowels) s) >= i
    where
        vowels = "aeiou"

containsRepeatedLetter :: String -> Bool 
containsRepeatedLetter (c:[])          = False
containsRepeatedLetter (c:cs@(c':_))   = if c == c' then True else containsRepeatedLetter cs


doesNotContainNaughtyStrings :: String -> Bool
doesNotContainNaughtyStrings s = and $ map (notInfixOf s) naughtys
    where
        naughtys = ["ab", "cd", "pq", "xy"]
        notInfixOf s s' = not $ isInfixOf s' s 


containsPairOfLettersTwice :: String -> Bool
containsPairOfLettersTwice (c:cs@(c':cs'))    = if isInfixOf (c:[c']) cs' then True else containsPairOfLettersTwice cs
containsPairOfLettersTwice _                  = False

containsRepeatedLetterWithPad :: String -> Bool
containsRepeatedLetterWithPad s@(c:cs) = if length s < 3 then False else (head s == s !! 2) || containsRepeatedLetterWithPad cs
