import Control.Monad (liftM)
import Data.Char (isLetter)

readInputLines :: String ->  IO [String]
readInputLines f = liftM lines (readFile f)

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

calculateMemoryAndCodeSizes :: String -> (String, Int, Int)
calculateMemoryAndCodeSizes s = (s, calculateMemorySize s, calculateCodeSize s)

calculateCodeAndEncodedSizes :: String -> (String, Int, Int)
calculateCodeAndEncodedSizes s = (s, calculateCodeSize s, (calculateCodeSize . encode) s)

secondIntMinusFirstInt :: Int -> (String, Int, Int) -> Int
secondIntMinusFirstInt size (_, f, s) = size + (s - f)


calculateMemorySize :: String -> Int
calculateMemorySize = f 0
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

quote :: String
quote = ['"']

encode :: String -> String
encode s = quote ++ e s ++ quote
    where
        e :: String -> String
        e []         = []
        e ('\\':cs)  = backslash ++ backslash ++ e cs
        e ('"':cs)   = escapedQuote ++ e cs
        e (c:cs)     = c:e cs

calculateCodeSize :: String -> Int
calculateCodeSize = length
