module Expr (module Expr) where

import Data.Map.Strict
import System.Random

data Aexpr = NumExpr Integer | VarExpr Char | 
             SumExpr Aexpr Aexpr | ProdExpr Aexpr Aexpr |
             RndExpr Aexpr | IntExpr Aexpr
data Sexpr = LiteralExpr String | ConcatExpr Sexpr Sexpr | ToStringExpr Aexpr
data Bexpr = EqExpr Aexpr Aexpr | GeExpr Aexpr Aexpr | LeExpr Aexpr Aexpr
data Com = LetCom Char Aexpr | PrintCom Sexpr | EndCom | GotoCom Integer |
           IfCom Bexpr Integer

-- prog represents the program as a map from a line number to the command at
-- that line number coupled with the number that follows it
data ProgEnv = ProgEnv {getProg :: Map Integer (Com, Integer)}
-- valMap is a map from a symbol (e.g. X) to the value stored at that symbol
data ProgState = ProgState {getPC :: Integer, 
                            getValMap :: Map Char Integer}

instance Show Com where
    show (LetCom x a) = "LET " ++ [x] ++ " = " ++ show a
    show (PrintCom x) = "PRINT " ++ show x
    show (EndCom) = "END"
    show (GotoCom i) = "GOTO " ++ show i
    show (IfCom b i) = "IF " ++ show b ++ " THEN " ++ show i

instance Show Aexpr where
    show (NumExpr x) = show x
    show (VarExpr x) = show x
    show (SumExpr x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (ProdExpr x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

instance Show Sexpr where
    show (LiteralExpr x) = "\"" ++ x ++ "\""
    show (ConcatExpr x y) = show x ++ "; " ++ show y
    show (ToStringExpr x) = show x

instance Show Bexpr where
    show (EqExpr a1 a2) = (show a1) ++ " = " ++ (show a2)
    show (GeExpr a1 a2) = (show a1) ++ " > " ++ (show a2)
    show (LeExpr a1 a2) = (show a1) ++ " < " ++ (show a2)

evalAexprInt :: Aexpr -> ProgState -> Either String Integer
evalAexprInt (NumExpr i) _ = Right i
evalAexprInt (VarExpr c) s = case res of
    Nothing -> Left ("Variable " ++ [c] ++ " is undefined.")
    Just i -> Right i
    where res = Data.Map.Strict.lookup c (getValMap s)
evalAexprInt (SumExpr a1 a2) m = (+) <$> (evalAexprInt a1 m) <*> (evalAexprInt a2 m)
evalAexprInt (ProdExpr a1 a2) m = (*) <$> (evalAexprInt a1 m) <*> (evalAexprInt a2 m)

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
