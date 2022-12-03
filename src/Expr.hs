module Expr (module Expr) where

import Data.Map.Strict
import System.Random
import Control.Monad.Reader
import Control.Monad.State.Lazy

data Aexpr = NumExpr Integer | VarExpr Char | 
             SumExpr Aexpr Aexpr | ProdExpr Aexpr Aexpr |
             RndExpr Aexpr | IntExpr Aexpr | FloatExpr Double
data Sexpr = LiteralExpr String | ConcatExpr Sexpr Sexpr | ToStringExpr Aexpr
data Bexpr = EqExpr Aexpr Aexpr | GeExpr Aexpr Aexpr | LeExpr Aexpr Aexpr
data Com = LetCom Char Aexpr | PrintCom Sexpr | EndCom | GotoCom Integer |
           IfCom Bexpr Integer | ForCom Char (Aexpr, Aexpr)

data Number = IntNum Integer | FloatNum Double deriving (Eq, Ord)

instance Show Number where
    show (IntNum i) = show i
    show (FloatNum i) = show i

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

toInt :: Number -> Number
toInt (IntNum i) = (IntNum i)
toInt (FloatNum i) = IntNum (round i)

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
data ProgState = ProgState {getPC :: Integer, 
                            getValMap :: Map Char Number,
                            getGen :: StdGen}

instance Show Com where
    show (LetCom x a) = "LET " ++ [x] ++ " = " ++ show a
    show (PrintCom x) = "PRINT " ++ show x
    show (EndCom) = "END"
    show (GotoCom i) = "GOTO " ++ show i
    show (IfCom b i) = "IF " ++ show b ++ " THEN " ++ show i
    show (ForCom c (i, j)) = "FOR " ++ [c] ++ " = " ++ show i ++ " TO " ++ show j

instance Show Aexpr where
    show (NumExpr x) = show x
    show (FloatExpr x) = show x
    show (VarExpr x) = show x
    show (SumExpr x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (ProdExpr x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (RndExpr x) = "RND(" ++ show x ++ ")"
    show (IntExpr x) = "INT(" ++ show x ++ ")"

instance Show Sexpr where
    show (LiteralExpr x) = "\"" ++ x ++ "\""
    show (ConcatExpr x y) = show x ++ "; " ++ show y
    show (ToStringExpr x) = show x

instance Show Bexpr where
    show (EqExpr a1 a2) = (show a1) ++ " = " ++ (show a2)
    show (GeExpr a1 a2) = (show a1) ++ " > " ++ (show a2)
    show (LeExpr a1 a2) = (show a1) ++ " < " ++ (show a2)

-- variables should not change during evaluation of an expression, so they are
-- in the reader
type Eval = ReaderT (Map Char Number) (State StdGen)

evalAexprRS :: Aexpr -> Eval (Either String Number)
evalAexprRS (NumExpr i) = return $ Right (IntNum i)
evalAexprRS (FloatExpr i) = return $ Right (FloatNum i)
evalAexprRS (VarExpr c) = do
    env <- ask
    let res = Data.Map.Strict.lookup c env
    case res of
        Nothing -> return $ Left ("Variable " ++ [c] ++ " is undefined.")
        Just i -> return $ Right i
evalAexprRS (SumExpr a1 a2) = do
    x <- evalAexprRS a1
    y <- evalAexprRS a2
    return $ addNums <$> x <*> y
evalAexprRS (ProdExpr a1 a2) = do
    x <- evalAexprRS a1
    y <- evalAexprRS a2
    return $ prodNums <$> x <*> y
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

evalAexprInt :: Aexpr -> ProgState -> Either String Number
evalAexprInt a s = evalState (runReaderT rs (getValMap s)) (getGen s)
    where rs = evalAexprRS a

evalAexpr = evalAexprInt

evalSexpr :: Sexpr -> ProgState -> Either String String
evalSexpr (LiteralExpr s) _ = Right s
evalSexpr (ConcatExpr s1 s2) m = (++) <$> (evalSexpr s1 m) <*> (evalSexpr s2 m)
evalSexpr (ToStringExpr a) m = show <$> evalAexpr a m

evalBexpr :: Bexpr -> ProgState -> Either String Bool
evalBexpr (EqExpr a1 a2) s = (==) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (GeExpr a1 a2) s = (>) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)
evalBexpr (LeExpr a1 a2) s = (<) <$> (evalAexpr a1 s) <*> (evalAexpr a2 s)

-- 10 LET A = 2
-- 20 LET B = 3
-- 30 LET C = 4
-- 40 PRINT A * (B + C)
-- 50 END

-- 20 INPUT H
-- 25 LET X = INT(RND(1)*H+1)
-- 27 PRINT X
-- 30 FOR I = 1 TO H
-- 35 PRINT I
-- 40 IF I = X THEN 60
-- 50 NEXT I
-- 60 END
