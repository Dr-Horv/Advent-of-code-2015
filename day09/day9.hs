import Data.List.Split
import Data.Char (isSpace)

readInputLines :: String ->  IO [String]
readInputLines f = readFile f >>= (\c -> (return . lines) c) 

main :: IO ()
main = do 
    ls <- readInputLines "input.txt"
    print $ head ls
    print $ readPath $ head ls
    return ()

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

readPath :: String -> (String, String, Int)
readPath s = (c1, c2, d)
    where 
        parts = map trim $ splitOn " to " s
        c1 = head parts
        c2 = takeUntilSpace $ parts !! 1
        ds = reverse $ takeUntilSpace $ reverse s
        d = read ds :: Int 

takeUntilSpace :: String -> String
takeUntilSpace = takeWhile (not . isSpace) 