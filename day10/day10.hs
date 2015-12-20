


main :: IO ()
main = do 
    print . length $ lookAndSay 50 "1113222113"
    return ()

lookAndSay :: Int -> String -> String
lookAndSay 0 s = s
lookAndSay n s = lookAndSay (n-1) $ f s
    
f :: String -> String
f []     = []
f s@(c:cs) = (show count) ++ (c:f cs')
    where
        count = countChar c s
        cs' = drop count s

countChar :: Char -> String -> Int
countChar c s = length $ takeWhile (c==) s