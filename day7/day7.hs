import qualified Data.Map as Map
import Data.Word
import Data.Bits
import Data.List.Split
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do 
    content <- readFile "input.txt"
    let ls = lines content
    putStrLn . show . length $ ls 
    let wires = map parseLineToWire ls
    let context = foldl f emptyContext wires
    putStrLn . show $ context
    let c' = buildCircuit context wires 
    putStrLn . show $ c'
    putStrLn . show $ lookupVariable c' "a"
    return ()

main2 :: IO ()
main2 = do 
    content <- readFile "input.txt"
    let ls = lines content
    putStrLn . show . length $ ls 
    let wires = map parseLineToWire ls
    let context = foldl f emptyContext wires
    let c' = buildCircuit context wires 
    putStrLn . show $ case lookupVariable c' "a" of
        Just v -> do    
            let newC = foldl f emptyContext wires
            let c'' = setConstToVar newC v "b"
            let c''' = buildCircuit c'' wires
            lookupVariable c''' "a"
        
    return ()

f :: Context -> Wire -> Context
f c (Const val var) = setConstToVar c val var
f c _               = c

buildCircuit :: Context -> [Wire] -> Context
buildCircuit c ws = if c /= c' then buildCircuit c' ws else c'
    where
        c' = foldl applyWire c ws  

type Var = String
data Exp = V Var | I Word16
    deriving Show
data BinaryGate = And Exp Exp | Or Exp Exp
    deriving Show
data ShiftOp = LSHIFT Var Int | RSHIFT Var Int
    deriving Show
data UnOp = Not Var 
    deriving Show
data Wire = Const Word16 Var | Gate BinaryGate Var | ShiftOperation ShiftOp Var | UnaryOp UnOp Var | Move Var Var
    deriving Show

type Context = Map.Map Var Word16

emptyContext :: Context
emptyContext = Map.fromList []



example :: Context  
example = Map.fromList   
    [("x", 123),
     ("y", 456)
    ] 

maybeReadWord16 :: String -> Maybe Word16
maybeReadWord16 = fmap fst . listToMaybe . reads

parseLineToWire :: String -> Wire
parseLineToWire s = parseWireType p1 p2
    where
        [p1, p2] = map trim $ splitOn "->" s

parseWireType :: String -> String -> Wire
parseWireType s v | "AND" `isInfixOf` s     = Gate (And (sToExp h) (sToExp l)) v
                  | "OR" `isInfixOf` s      = Gate (Or (sToExp h) (sToExp l)) v
                  | "NOT" `isInfixOf` s     = UnaryOp (Not l) v
                  | "LSHIFT" `isInfixOf` s  = ShiftOperation (LSHIFT h ((read l) :: Int)) v
                  | "RSHIFT" `isInfixOf` s  = ShiftOperation (RSHIFT h ((read l) :: Int)) v
                  | otherwise               = case maybeReadWord16 h of
                                                Just i -> Const (read h :: Word16) v
                                                Nothing -> Move h v 
                                                    
    where
        parts = words s
        h = (trim . head) parts
        l = (trim . last) parts
        sToExp :: String -> Exp
        sToExp s = case maybeReadWord16 s of
                        Just i  -> I i
                        Nothing -> V s


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

applyWire :: Context -> Wire -> Context
applyWire c (Gate g v)              = case applyGate c g of
                                        Just i  -> setConstToVar c i v
                                        Nothing -> c
applyWire c (ShiftOperation o v)    = case applyShiftOp c o of
                                        Just i  -> setConstToVar c i v
                                        Nothing -> c 
applyWire c (UnaryOp o v)           = case applyUnaryOp c o of
                                        Just i  -> setConstToVar c i v
                                        Nothing -> c
applyWire c (Move v1 v2)            = case lookupVariable c v1 of
                                        Just i  ->  setConstToVar c i v2
                                        Nothing ->  c
applyWire c _                       = c

applyGate :: Context -> BinaryGate -> Maybe Word16
applyGate c (And e1 e2) = case lookupExps c [e1, e2] of
                                Just [v1, v2]   -> Just $ v1 .&. v2
                                Nothing         -> Nothing
applyGate c (Or e1 e2) = case lookupExps c [e1, e2] of
                                Just [v1, v2]   -> Just $ v1 .|. v2
                                Nothing         -> Nothing

applyShiftOp :: Context -> ShiftOp -> Maybe Word16
applyShiftOp c (LSHIFT v n) = case lookupVariable c v of
                                Just v' -> Just $ shiftL v' n
                                Nothing -> Nothing
applyShiftOp c (RSHIFT v n) = case lookupVariable c v of
                                Just v' -> Just $ shiftR v' n
                                Nothing -> Nothing

applyUnaryOp :: Context -> UnOp -> Maybe Word16
applyUnaryOp c (Not v) = case lookupVariable c v of
                            Just v' -> Just $ complement v'
                            Nothing -> Nothing

setConstToVar :: Context -> Word16 -> Var -> Context
setConstToVar c i v = Map.insertWith mf v i c
    where
        mf :: a -> a -> a
        mf a _ = a

lookupExps :: Context -> [Exp] -> Maybe [Word16]
lookupExps c es = mapM (expToWord c) es
    where
        expToWord :: Context -> Exp -> Maybe Word16
        expToWord c (I w) = Just w
        expToWord c (V v) = lookupVariable c v 

lookupVariable :: Context -> Var -> Maybe Word16
lookupVariable c v = Map.lookup v c 

lookupVariables :: Context -> [Var] -> Maybe [Word16]
lookupVariables c vs = mapM (lookupVariable c) vs

