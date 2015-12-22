import Control.Monad (liftM)
import Data.List(sortOn)

readInputLines :: String ->  IO [String]
readInputLines f = liftM lines (readFile f)

main :: IO ()
main = do 
    ls <- readInputLines "input.txt"
    let rainderrInfos = map parseLine ls
    let deerWithDistance = map (calculateDistanceOfRaindeer 2503) rainderrInfos
    let winner = decideWinner deerWithDistance
    print winner
    return ()

main2 :: IO ()
main2 = do 
    ls <- readInputLines "input.txt"
    let rainderrInfos = map parseLine ls
    let seconds = [1..2503]
    let deersWithDistancesOfAllTimes = map (\n -> map (calculateDistanceOfRaindeer n) rainderrInfos) seconds
    let leadersEachSecond = map decideLeaders deersWithDistancesOfAllTimes
    let score = calculatePointsForRaindeers leadersEachSecond rainderrInfos
    let sortedScore = reverse $ sortOn snd score
    print sortedScore
    return ()

type RaindeerInfo = (String, Int, Int, Int)

calculateDistanceOfRaindeer :: Int -> RaindeerInfo -> (String, Rational)
calculateDistanceOfRaindeer n ri@(name, _, _, _) = (name, calculateDistanceOfRaindeedAfterNSeconds n ri) 

calculatePointsForRaindeers :: [[(String, a)]] -> [RaindeerInfo] -> [(String, Int)]
calculatePointsForRaindeers ls ris = foldl f startScore ls
    where
        startScore = map (\(s, _, _, _) -> (s, 0)) ris 
        f :: [(String, Int)] -> [(String, a)] -> [(String, Int)]
        f = foldl f' 
        f' :: [(String, Int)] -> (String, a) -> [(String, Int)]
        f' state leader = updateItemWithMergeBy (\i i' -> fst i == fst i') (\_ (n, p) -> (n, p+1)) leader state

updateItemWithMergeBy :: Ord a => (a -> b -> Bool) -> (b -> a -> a) -> b -> [a] -> [a]
updateItemWithMergeBy _ _ _ [] = []
updateItemWithMergeBy cmpf mf i' (i:is) = if cmpf i i' then mf i' i:is else i:updateItemWithMergeBy cmpf mf i' is

decideLeaders :: [(String, Rational)] -> [(String, Rational)]
decideLeaders = foldl f [("None", 0)]
    where
        f :: [(String, Rational)] -> (String, Rational) -> [(String, Rational)]
        f curr@((_, d) : _) next@(_, d')
          | d > d' = curr
          | d' > d = [next]
          | otherwise = next : curr


decideWinner :: [(String, Rational)] -> (String, Rational)
decideWinner = foldl f ("None", 0)
    where
        f :: (String, Rational) -> (String, Rational) -> (String, Rational)
        f curr@(_, d) next@(_, d') = if d >= d' then curr else next

parseLine :: String -> RaindeerInfo
parseLine s = (name, speed, runTime, restTime)
    where
        parts = words s
        name = parts !! 0
        speed = read (parts !! 3) :: Int
        runTime = read (parts !! 6) :: Int 
        restTime = read (parts !! 13) :: Int

calculateDistanceOfRaindeedAfterNSeconds :: Int -> RaindeerInfo -> Rational
calculateDistanceOfRaindeedAfterNSeconds n (_, speed, runTime, restTime) = realToFrac(runs * speed * runTime) + currRunDist
    where
        cycleTime = fromIntegral runTime + fromIntegral restTime
        runThreshold = fromIntegral runTime / cycleTime :: Rational
        cycles = fromIntegral n / cycleTime :: Rational
        wholeCycles = truncate cycles :: Int
        cyclesFraction = cycles - fromIntegral wholeCycles :: Rational
        runs = wholeCycles + if cyclesFraction >= runThreshold then 1 else 0
        currRunDist = if cyclesFraction < runThreshold then calculateFracRun cyclesFraction runThreshold runTime speed else 0

calculateFracRun :: Rational -> Rational -> Int -> Int -> Rational
calculateFracRun cyclesFraction runThreshold runTime speed = dist
    where
        frac = cyclesFraction/runThreshold
        dist = frac * fromIntegral runTime * fromIntegral speed

