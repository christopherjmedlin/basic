module Expr (module Expr) where

import Prelude hiding (lookup)
import Data.Map.Strict hiding ((!))
import System.Random
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Ix
import Data.Array
import Data.Maybe (fromJust)
import Data.Either

data Aexpr = NumExpr Integer | VarExpr Char |
             SumExpr Aexpr Aexpr | ProdExpr Aexpr Aexpr |
             RndExpr Aexpr | IntExpr Aexpr | FloatExpr Double |
             DivExpr Aexpr Aexpr | DiffExpr Aexpr Aexpr |
             ArrExpr Char (Aexpr, Aexpr, Aexpr, Aexpr)
-- NoNewLineExpr is to signal to the interpreter in the event that there is a
-- semicolon at the end of the expression
data Sexpr = LiteralExpr String | ConcatExpr Sexpr Sexpr | ToStringExpr Aexpr |
             NoNewLineExpr Sexpr | TabExpr Integer | NoNewTabExpr Sexpr |
             ConcatTabExpr Sexpr Sexpr
data Bexpr = EqExpr Aexpr Aexpr | GeExpr Aexpr Aexpr | LeExpr Aexpr Aexpr |
             GeqExpr Aexpr Aexpr | LeqExpr Aexpr Aexpr | NeqExpr Aexpr Aexpr |
             AndExpr Bexpr Bexpr | OrExpr Bexpr Bexpr
data Com = LetCom Aexpr Aexpr | PrintCom Sexpr | EndCom | GotoCom Integer |
           IfCom Bexpr Integer | ForCom Char (Aexpr, Aexpr, Aexpr) | NextCom [Char] |
           InputCom String [Char] | GoSubCom Integer | ReturnCom | SeqCom Com Com |
           DimCom [Aexpr] | RemCom | OnGotoCom Char [Integer]

data Number = IntNum Integer | FloatNum Double deriving (Eq, Ord)

instance Show Number where
    show (IntNum i) = show i
    show (FloatNum i) = show i

type ArrIndex = (Integer, Integer, Integer, Integer)
type Arr = Array ArrIndex Number

-- TODO could probably clean this up with a higher order function
addNums :: Number -> Number -> Number
addNums (IntNum i) (FloatNum d) = FloatNum (d + (fromIntegral i))
addNums (FloatNum d) (IntNum i) = FloatNum (d + (fromIntegral i))
addNums (IntNum i) (IntNum j) = IntNum (i+j)
addNums (FloatNum i) (FloatNum j) = FloatNum (i+j)

prodNums :: Number -> Number -> Number
prodNums (IntNum i) (FloatNum d) = FloatNum (d * (fromIntegral i))
prodNums (FloatNum d) (IntNum i) = FloatNum (d * (fromIntegral i))
prodNums (IntNum i) (IntNum j) = IntNum (i*j)
prodNums (FloatNum i) (FloatNum j) = FloatNum (i*j)

divNums :: Number -> Number -> Number
divNums (IntNum i) (FloatNum d) = FloatNum (d / (fromIntegral i))
divNums (FloatNum d) (IntNum i) = FloatNum (d / (fromIntegral i))
divNums (IntNum i) (IntNum j) = IntNum (quot i j)
divNums (FloatNum i) (FloatNum j) = FloatNum (i/j)

subNums :: Number -> Number -> Number
subNums (IntNum i) (FloatNum d) = FloatNum (d - (fromIntegral i))
subNums (FloatNum d) (IntNum i) = FloatNum (d - (fromIntegral i))
subNums (IntNum i) (IntNum j) = IntNum (i - j)
subNums (FloatNum i) (FloatNum j) = FloatNum (i - j)

toInt :: Number -> Number
toInt (IntNum i) = (IntNum i)
toInt (FloatNum i) = IntNum (floor i)

toNormalInt :: Number -> Integer
toNormalInt (IntNum i) = i
toNormalInt x = (toNormalInt.toInt) x

randomNum :: StdGen -> Number -> (Number, StdGen)
randomNum g (FloatNum i) = (FloatNum n, newg)
    where (n, newg) = randomR (0, i) g
randomNum g (IntNum 1) = randomNum g (FloatNum 1.0)
randomNum g (IntNum i) = (IntNum n, newg)
    where (n, newg) = randomR (0, i) g

-- prog represents the program as a map from a line number to the command at
-- that line number coupled with the number that follows it
data ProgEnv = ProgEnv {getProg :: Map Integer (Com, Integer)}
-- valMap is a map from a symbol (e.g. X) to the value stored at that symbol
-- iters are the iterator variables mapped to their upper bounds and their
-- return address
-- stack is the subroutine return address stack
-- gotoFlag indicates to the interpreter when a goto has happened, to avoid
-- going to the next line after it
data ProgState = ProgState {getPC :: Integer,
                            getValMap :: Map Char Number,
                            getGen :: StdGen,
                            getIters :: Map Char (Number, Integer, Number),
                            getStack :: [Integer],
                            gotoFlag :: Bool,
                            getArrMap :: Map Char Arr, 
                            skipFlag :: Bool}

putPC :: Integer -> ProgState -> ProgState
putPC i s = ProgState i (getValMap s)
                        (getGen s)
                        (getIters s)
                        (getStack s)
                        (gotoFlag s)
                        (getArrMap s)
                        (skipFlag s)

insertVal :: Char -> Number -> ProgState -> ProgState
insertVal c n (ProgState pc v g i s go a sk) = ProgState pc (insert c n v) g i s go a sk

delVal :: Char -> ProgState -> ProgState
delVal c (ProgState pc v g i s go a sk) = ProgState pc (delete c v) g i s go a sk

putGen :: StdGen -> ProgState -> ProgState
putGen g (ProgState pc v _ i s go a sk) = ProgState pc v g i s go a sk

insertIter :: Char -> Number -> Integer -> Number -> ProgState -> ProgState
insertIter c n1 n2 n3 (ProgState pc v g i s go a sk) = ProgState pc v g (insert c (n1, n2, n3) i) s go a sk

pushStack :: Integer -> ProgState -> ProgState
pushStack i (ProgState p v g it s go a sk) = ProgState p v g it (i : s) go a sk

peekStack :: ProgState -> Integer
peekStack (ProgState _ _ _ _ s _ _ _) = head s

-- doesn't return the result, gotta use peek for that
popStack :: ProgState -> ProgState
popStack (ProgState p v g i s go a sk) = ProgState p v g i (tail s) go a sk

setGoto :: ProgState -> ProgState
setGoto (ProgState p v g it s _ a sk) = ProgState p v g it s True a sk

unsetGoto :: ProgState -> ProgState
unsetGoto (ProgState p v g it s _ a sk) = ProgState p v g it s False a sk

newArr :: Char -> ArrIndex -> ProgState -> ProgState
newArr c dim (ProgState p v g it s go a sk) = ProgState p v g it s go new sk
    where start = (0,0,0,0)
          end = dim
          arr = array (start,end) []
          new = insert c (arr // [(i,(IntNum 0)) | i <- indices arr]) a

insertArr :: Char -> ArrIndex -> Number -> ProgState -> ProgState
insertArr c ix val (ProgState p v g it s go a sk) = ProgState p v g it s go new sk
    where oldarr = fromJust $ lookup c a
          newarr = oldarr // [(ix, val)]
          new = insert c newarr a

getArr :: Char -> ArrIndex -> Map Char Arr -> Number
getArr c ix arrs = (fromJust (lookup c arrs)) ! ix

setSkip :: ProgState -> ProgState
setSkip (ProgState p v g it s go a _) = ProgState p v g it s go a True

unsetSkip :: ProgState -> ProgState
unsetSkip (ProgState p v g it s go a sk) = ProgState p v g it s go a False

instance Show Com where
    show (LetCom x a) = "LET " ++ show x ++ " = " ++ show a
    show (PrintCom x) = "PRINT " ++ show x
    show (EndCom) = "END"
    show (GotoCom i) = "GOTO " ++ show i
    show (IfCom b i) = "IF " ++ show b ++ " THEN " ++ show i
    show (ForCom c (i, j, k)) = "FOR " ++ [c] ++ " = " ++ show i
                                ++ " TO " ++ show j ++ " STEP " ++ show k
    show (NextCom c) = "NEXT " ++ show [c]
    show (InputCom "" c) = "INPUT " ++ show c
    show (InputCom s c) = "INPUT \"" ++ s ++ "\"; " ++ show c
    show (GoSubCom i) = "GOSUB " ++ show i
    show (ReturnCom) = "RETURN"
    show (SeqCom c1 c2) = show c1 ++ " : " ++ show c2
    show (DimCom as) = "DIM " ++ show as
    show RemCom = ""
    show (OnGotoCom c is) = "ON " ++ [c] ++ " GOTO " ++ show is

instance Show Aexpr where
    show (NumExpr x) = show x
    show (FloatExpr x) = show x
    show (VarExpr x) = show x
    show (SumExpr x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (ProdExpr x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (RndExpr x) = "RND(" ++ show x ++ ")"
    show (IntExpr x) = "INT(" ++ show x ++ ")"
    show (DivExpr x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (DiffExpr x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (ArrExpr c xs) = c : show xs

instance Show Sexpr where
    show (LiteralExpr x) = "\"" ++ x ++ "\""
    show (ConcatExpr x y) = show x ++ "; " ++ show y
    show (ToStringExpr x) = show x
    show (NoNewLineExpr x) = show x ++ ";"
    show (NoNewTabExpr x) = show x ++ ","
    show (ConcatTabExpr x y) = show x ++ ", " ++ show y
    show (TabExpr i) = "TAB(" ++ show i ++ ")"

instance Show Bexpr where
    show (EqExpr a1 a2) = (show a1) ++ " = " ++ (show a2)
    show (GeExpr a1 a2) = (show a1) ++ " > " ++ (show a2)
    show (LeExpr a1 a2) = (show a1) ++ " < " ++ (show a2)
    show (NeqExpr a1 a2) = (show a1) ++ " <> " ++ (show a2)
    show (LeqExpr a1 a2) = (show a1) ++ " <= " ++ (show a2)
    show (GeqExpr a1 a2) = (show a1) ++ " >= " ++ (show a2)
    show (AndExpr a1 a2) = (show a1) ++ " AND " ++ (show a2)
    show (OrExpr a1 a2) = (show a1) ++ " OR " ++ (show a2)

-- variables should not change during evaluation of an expression, so they are
-- in the reader
type Eval = ReaderT (Map Char Number, Map Char Arr) (State StdGen)

evalAexprRS :: Aexpr -> Eval (Either String Number)
evalAexprRS (NumExpr i) = return $ Right (IntNum i)
evalAexprRS (FloatExpr i) = return $ Right (FloatNum i)
evalAexprRS (VarExpr c) = do
    env <- fst <$> ask
    let res = Data.Map.Strict.lookup c env
    case res of
        Nothing -> return $ Left ("Variable " ++ [c] ++ " is undefined.")
        Just i -> return $ Right i
evalAexprRS (ArrExpr c (a1,a2,a3,a4)) = do
    env <- snd <$> ask
    i1 <- (toNormalInt.(fromRight (IntNum 0))) <$> evalAexprRS a1
    i2 <- (toNormalInt.(fromRight (IntNum 0))) <$> evalAexprRS a2
    i3 <- (toNormalInt.(fromRight (IntNum 0))) <$> evalAexprRS a3
    i4 <- (toNormalInt.(fromRight (IntNum 0))) <$> evalAexprRS a4
    let res = getArr c (i1,i2,i3,i4) env
    return $ Right res
-- TODO modify this to just use applicative instance of ReaderT
evalAexprRS (SumExpr a1 a2) = do
    x <- evalAexprRS a1
    y <- evalAexprRS a2
    return $ addNums <$> x <*> y
evalAexprRS (ProdExpr a1 a2) = do
    x <- evalAexprRS a1
    y <- evalAexprRS a2
    return $ prodNums <$> x <*> y
evalAexprRS (DivExpr a1 a2) = do
    x <- evalAexprRS a1
    y <- evalAexprRS a2
    return $ divNums <$> x <*> y
evalAexprRS (DiffExpr a1 a2) = do
    x <- evalAexprRS a1
    y <- evalAexprRS a2
    return $ subNums <$> x <*> y
evalAexprRS (IntExpr a) = do
    x <- evalAexprRS a
    return $ toInt <$> x
evalAexprRS (RndExpr a) = do
    g <- get
    x <- evalAexprRS a
    case x of
        Left s -> return $ Left s
        Right i -> do
            let (n, newg) = randomNum g i
            put newg
            return $ Right n

runEvalAexpr :: Aexpr -> ProgState -> (Either String Number, StdGen)
runEvalAexpr a s = runState (runReaderT rs
                        (getValMap s, getArrMap s)) (getGen s)
    where rs = evalAexprRS a

evalAexpr a s = fst (runEvalAexpr a s)

evalSexpr :: Sexpr -> ProgState -> Either String String
evalSexpr (LiteralExpr s) _ = Right s
evalSexpr (ConcatExpr s1 s2) m = (++) <$> (evalSexpr s1 m) <*> (evalSexpr s2 m)
evalSexpr (ConcatTabExpr s1 s2) m = (++ "\t") <$> ((++) <$> (evalSexpr s1 m) <*> (evalSexpr s2 m))
evalSexpr (ToStringExpr a) m = (++ " ") . show <$> evalAexpr a m
evalSexpr (TabExpr i) _  = Right (replicate (fromIntegral i) ' ')

evalBexpr :: Bexpr -> ProgState -> Either String Bool
evalBexpr (EqExpr a1 a2) s = (==) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (GeExpr a1 a2) s = (>) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (LeExpr a1 a2) s = (<) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (NeqExpr a1 a2) s = (/=) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (LeqExpr a1 a2) s = (<=) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (GeqExpr a1 a2) s = (>=) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (AndExpr b1 b2) s = (&&) <$> (evalBexpr b1 s) <*> (evalBexpr b1 s)
evalBexpr (OrExpr b1 b2) s = (||) <$> (evalBexpr b1 s) <*> (evalBexpr b1 s)
