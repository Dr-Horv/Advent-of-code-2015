

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
    let ls = [ls' !! 3]
    print ls
    print $ encode $ head ls
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

quote :: String
quote = '\\':['"']

encode :: String -> String
encode s = quote ++ (e s) ++ quote
    where 
        e :: String -> String
        e []     = [] 
        e (c:cs) = case c of 
                            '\\' -> if ((=='x') . head) cs 
                                then  quote ++ (take 3 cs) ++ e (drop 3 cs)
                                else  quote ++ [c] ++ e cs
                            c     -> c:(e cs)

calculateCodeSize :: String -> Int
calculateCodeSize = length