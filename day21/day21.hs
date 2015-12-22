import Data.List  (permutations)

type Item = (String, Int, Int, Int)
data Armor = Armor Item deriving (Show, Eq)
data Ring = Ring Item deriving (Show, Eq)
data Weapon = Weapon Item deriving (Show, Eq)
data Gear = W Weapon | A Armor | R Ring deriving (Show, Eq)

main :: IO ()
main = do 
    let costWithGear = testAndBundleWithCostWin getPermutations
    let cheapest = findCheapest costWithGear
    print cheapest
    return ()

main2 :: IO ()
main2 = do 
    let costWithGear = testAndBundleWithCostLose getPermutations
    let mostExpensive = findMostExpensive costWithGear
    print mostExpensive
    return ()

findCheapest :: [(Int, [Gear])] -> (Int, [Gear])
findCheapest = foldl f (999999, []) 
    where
        f :: (Int, [Gear]) -> (Int, [Gear]) -> (Int, [Gear])
        f o@(v, g) s@(v', g') = if v < v' then o else s

findMostExpensive :: [(Int, [Gear])] -> (Int, [Gear])
findMostExpensive = foldl f (-1, []) 
    where
        f :: (Int, [Gear]) -> (Int, [Gear]) -> (Int, [Gear])
        f o@(v, g) s@(v', g') = if v > v' then o else s

testAndBundleWithCostWin :: [[Gear]] -> [(Int, [Gear])]
testAndBundleWithCostWin = testAndBundleWithCostWinOrLose False

testAndBundleWithCostLose :: [[Gear]] -> [(Int, [Gear])]
testAndBundleWithCostLose = testAndBundleWithCostWinOrLose True 

testAndBundleWithCostWinOrLose :: Bool -> [[Gear]] -> [(Int, [Gear])]
testAndBundleWithCostWinOrLose _ []     = []
testAndBundleWithCostWinOrLose b (g:gs) = if test 
        then (calculateCostOfGear char, g):rest 
        else rest
    where 
        test = if b then not t else t
        rest = testAndBundleWithCostWinOrLose b gs
        (t, char) = testSet g

testSet :: [Gear] -> (Bool, Character)
testSet g = (doFight getBoss char, char)
    where
        char = createChar g


createChar :: [Gear] -> Character
createChar g = char 
    where       
        W w = head [ x | x@(W _) <- g]
        A a = head [ x | x@(A _) <- g]
        [R rr, R lr] = [ x | x@(R _) <- g]
        char = Character {  charHp = 100, 
                            weapon = w,
                            armor = a,
                            leftRing = lr,
                            rightRing = rr }

getPermutations :: [[Gear]]
getPermutations = [ tupleToList (w, a, lr, rr) | w <- weapons, a <- armors, lr <- rings, rr <- rings, lr /= rr ]
    where
        rings = map R getAvailableRings
        armors = map A getAvailableArmor
        weapons = map W getAvailableWeapons

tupleToList :: (a, a, a, a) -> [a]
tupleToList (a, b, c, d) = [a,b,c,d]

checkSet :: [Gear] -> Bool
checkSet g = f g (0, 0, 0)
    where
        f :: [Gear] -> (Int, Int, Int) -> Bool
        f [] (wn, _, _)         = wn == 1
        f (W _:gs) (wn, a, r) = (wn == 0) && f gs (wn + 1, a, r)
        f (A _:gs) (w, an, r) = (an == 0) && f gs (w, an + 1, r)
        f (R _:gs) (w, a, rn) = (rn <= 1) && f gs (w, a, rn + 1)

getAvailableRings :: [Ring]
getAvailableRings = map Ring [   ("None", 0, 0, 0),
                        ("None2", 0, 0, 0),
                        ("Dmg +1", 25, 1, 0),
                        ("Dmg +2", 50, 2, 0),
                        ("Dmg +3", 100, 3, 0),
                        ("Dfs +1", 20, 0, 1),
                        ("Dfs +1", 40, 0, 2),
                        ("Dfs +1", 80, 0, 3)
                    ]

getAvailableArmor :: [Armor]
getAvailableArmor = map Armor [   
                        ("None",         0, 0, 0),
                        ("Leather",     13, 0, 1),
                        ("Chainmail",   31, 0, 2),
                        ("Splintmail",  53, 0, 3),
                        ("Bandedmail",  75, 0, 4),
                        ("Platemail",  102, 0, 5)
                    ]

getAvailableWeapons :: [Weapon]
getAvailableWeapons = map Weapon [
                        ("Dagger",       8, 4, 0),
                        ("Shortsword",  10, 5, 0),
                        ("Warhammer",   25, 6, 0),
                        ("Longsword",   40, 7, 0),
                        ("Greataxe",    74, 8, 0)
                      ]

doFight :: Boss -> Character -> Bool
doFight b c | hp b <= 0     = True 
            | charHp c <= 0 = False
            | otherwise     = doFight b' c' 
    where 
        gear = getGear c
        charDmg = getDmg gear - bossArmor b
        b' = decreaseBossHp charDmg b
        c' = decreaseCharHp (dmg b - getDfs gear) c 

decreaseBossHp :: Int -> Boss -> Boss
decreaseBossHp i b = Boss { hp = hp b - i,
                              dmg = dmg b,
                              bossArmor = bossArmor b
                          }

decreaseCharHp :: Int -> Character -> Character
decreaseCharHp i c = Character { 
                            charHp = charHp c - i, 
                            weapon = weapon c,
                            armor = armor c,
                            leftRing = leftRing c,
                            rightRing = rightRing c 
                          }
getDmg :: [Item] -> Int
getDmg = sum . map getDmgOfItem

getDfs :: [Item] -> Int
getDfs = sum . map getDfsOfItem

getDmgOfItem :: Item -> Int
getDmgOfItem (_, _, d, _) = d

getDfsOfItem :: Item -> Int
getDfsOfItem (_, _, _, d) = d

getGear :: Character -> [Item]
getGear c = [w, a, lr, rr]
    where 
        Weapon w = weapon c
        Armor a = armor c
        Ring lr = leftRing c
        Ring rr = rightRing c

getBoss :: Boss
getBoss = Boss { hp = 103, 
                 dmg = 9, 
                 bossArmor = 2}

getDummyChar :: Character
getDummyChar = Character {  charHp = 100, 
                            weapon = head getAvailableWeapons,
                            armor = head getAvailableArmor,
                            leftRing = head getAvailableRings,
                            rightRing = last getAvailableRings  }

calculateCostOfGear :: Character -> Int
calculateCostOfGear c = sum $ map getCostOfItem $ getGear c

getCostOfItem :: Item -> Int
getCostOfItem (_, c, _, _) = c

data Boss = Boss { hp :: Int
                ,dmg :: Int
                ,bossArmor :: Int 
                } deriving (Show)

data Character = Character { charHp :: Int 
                     ,weapon :: Weapon 
                     ,armor :: Armor  
                     ,leftRing :: Ring  
                     ,rightRing :: Ring 
                     } deriving (Show)  
