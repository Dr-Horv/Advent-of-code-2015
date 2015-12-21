
import Data.List (group)

main :: IO ()
main = do 
    print . length $ lookAndSay 50 "1113222113"
    return ()

lookAndSay :: Int -> String -> String
lookAndSay 0 s = s
lookAndSay n s = lookAndSay (n-1) $ f' s

f' :: String -> String
f' s = concatMap m gs
    where
        gs = group s
        m :: String -> String
        m s = (show . length) s ++ [head s] 