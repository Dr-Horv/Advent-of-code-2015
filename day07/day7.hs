import qualified Data.Map as Map
import Data.Word
import Data.Bits
import Data.List.Split
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe, fromMaybe)

main :: IO ()
main = do 
    content <- readFile "input.txt"
    let ls = lines content
    print . length $ ls 
    let wires = map parseLineToWire ls
    let context = foldl f emptyContext wires
    print context
    let c' = buildCircuit context wires 
    print c'
    print $ lookupVariable c' "a"
    return ()

main2 :: IO ()
main2 = do 
    content <- readFile "input.txt"
    let ls = lines content
    print . length $ ls 
    let wires = map parseLineToWire ls
    let context = foldl f emptyContext wires
    let c' = buildCircuit context wires 
    print $ case lookupVariable c' "a" of
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
                  | "LSHIFT" `isInfixOf` s  = ShiftOperation (LSHIFT h (read l :: Int)) v
                  | "RSHIFT" `isInfixOf` s  = ShiftOperation (RSHIFT h (read l :: Int)) v
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
applyWire c (Gate g v)              = updateIfFound c (applyGate c g) v
applyWire c (ShiftOperation o v)    = updateIfFound c (applyShiftOp c o) v 
applyWire c (UnaryOp o v)           = updateIfFound c (applyUnaryOp c o) v
applyWire c (Move v1 v2)            = updateIfFound c (lookupVariable c v1) v2
applyWire c _                       = c

updateIfFound :: Context -> Maybe Word16 -> Var -> Context
updateIfFound c m v = case m of
                        Just i  -> setConstToVar c i v
                        Nothing -> c

applyGate :: Context -> BinaryGate -> Maybe Word16
applyGate c (And e1 e2) = applyBinFun c [e1, e2] (.&.)
applyGate c (Or e1 e2) = applyBinFun c [e1, e2] (.|.)

applyShiftOp :: Context -> ShiftOp -> Maybe Word16
applyShiftOp c (LSHIFT v n) = applyFun c v (`shiftL` n)
applyShiftOp c (RSHIFT v n) = applyFun c v (`shiftR` n)

applyUnaryOp :: Context -> UnOp -> Maybe Word16
applyUnaryOp c (Not v) = applyFun c v complement

applyFun :: Context -> Var -> (Word16 -> Word16) -> Maybe Word16
applyFun c v f = case lookupVariable c v of
                    Just v' -> Just $ f v'
                    Nothing -> Nothing 

applyBinFun :: Context -> [Exp] -> (Word16 -> Word16 -> Word16) -> Maybe Word16
applyBinFun c [e1, e2] f = case lookupExps c [e1, e2] of
                                Just [v1, v2]   -> Just $ f v1 v2
                                Nothing         -> Nothing

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

