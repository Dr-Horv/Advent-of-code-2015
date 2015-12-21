
import Data.List (isInfixOf, group)

alphabeth = "abcdefghijklmnopqrstuvwxyz"
alphabeth' = "abcdefghjkmnpqrstuvwxyz"


main:: IO ()
main = do
    print $ findNextValidPassword "vzbxkghb"
    return ()

main2 :: IO ()
main2 = do
    print $ findNextValidPassword $ findNextValidPassword "vzbxkghb"
    return ()

findNextValidPassword :: String -> String
findNextValidPassword s = if testPassword s' then s' else findNextValidPassword s'
    where
        s' = nextPassword s

testPassword :: String -> Bool
testPassword s = checkContainConsecetiveThreeChar s && containsTwoOcurrencesOFRepeatedLetter s

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar c   = if not (null c') then head c' else error $ "nextChar got empty " ++ show c
   where
    c' = drop 1 $ dropWhile (/=c) alphabeth' 

nextPassword :: String -> String
nextPassword "zzzzzzzz" = error "Exausted all passwords"
nextPassword s = if lastChar /= 'z' 
                    then sWithoutLast ++ [lastChar']
                    else  nextPassword sWithoutLast ++ [lastChar']
    where
        s' = reverse s
        sWithoutLast = (reverse . tail) s'
        lastChar = head s'
        lastChar' = nextChar lastChar

containsTwoOcurrencesOFRepeatedLetter :: String -> Bool 
containsTwoOcurrencesOFRepeatedLetter s = ((>=2) . length) $ filter ((>1) . length) $ group s

checkContainConsecetiveThreeChar :: String -> Bool
checkContainConsecetiveThreeChar s = (length s >= 3) && (c s || checkRest)
    where
        checkRest = checkContainConsecetiveThreeChar $ tail s
        c :: String -> Bool
        c s = take 3 s `isInfixOf` alphabeth'
