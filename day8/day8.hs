

import Data.Char (isLetter)

readInputLines :: String ->  IO [String]
readInputLines f = readFile f >>= (\c -> (return . lines) c) 

main :: IO ()
main = do 
    ls <- readInputLines "input.txt"
    print $ foldl secondIntMinusFirstInt 0 $ map calculateMemoryAndCodeSizes ls
    return ()

main2 :: IO ()
main2 = do
    ls <- readInputLines "input.txt"
    print $ foldl secondIntMinusFirstInt 0 $ map calculateCodeAndEncodedSizes ls
    return ()

main' :: IO ()
main' = do
    ls' <- readInputLines "example.txt"
    let ls = [ls' !! 0]
    print ls
    let e = encode $ head ls
    writeFile "derp.txt" $ unlines $ map encode ls'
    print e
    let mapped = map calculateCodeAndEncodedSizes ls'
    print mapped
    print $ foldl secondIntMinusFirstInt 0 $ mapped
    return ()

calculateMemoryAndCodeSizes :: String -> (String, Int, Int)
calculateMemoryAndCodeSizes s = (s, calculateMemorySize s, calculateCodeSize s)

calculateCodeAndEncodedSizes :: String -> (String, Int, Int)
calculateCodeAndEncodedSizes s = (s, calculateCodeSize s, (calculateCodeSize . encode) s)

secondIntMinusFirstInt :: Int -> (String, Int, Int) -> Int
secondIntMinusFirstInt size (_, f, s) = size + (s - f)


calculateMemorySize :: String -> Int
calculateMemorySize s = f 0 s
    where
        f :: Int -> String -> Int
        f n []       = n
        f n ('"':cs) = f n cs
        f n (c:cs)   = case c of 
                            '\\' -> if ((=='x') . head) cs 
                                then f (n+1) (drop 3 cs) 
                                else f (n+1) (drop 1 cs)
                            c     -> f (n+1) cs

backslash :: String
backslash = ['\\']

escapedQuote :: String
escapedQuote = '\\':quote

escape :: String -> String
escape s = '\\':s

quote :: String
quote = ['"']

encode :: String -> String
encode s = quote ++ (e s) ++ quote
    where 
        e :: String -> String
        e []     = [] 
        e (c:cs) = case c of 
                            '\\' -> if ((=='x') . head) cs 
                                then  '\\':(escape . take 3) cs ++ e (drop 3 cs)
                                else  escape [c] ++ e cs
                            '"' -> escapedQuote ++ e cs
                            c     -> c:(e cs)

calculateCodeSize :: String -> Int
calculateCodeSize = length