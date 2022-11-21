module Expr (module Expr) where

import Data.Map.Strict

data Com = LetCom String Aexpr | PrintCom Sexpr
data Aexpr = NumExpr Integer | VarExpr Char | SumExpr Aexpr Aexpr | ProdExpr Aexpr Aexpr
data Sexpr = LiteralExpr String | ConcatExpr Sexpr Sexpr | ToStringExpr Aexpr

instance Show Com where
    show (LetCom x a) = "LET " ++ x ++ show a
    show (PrintCom x) = "PRINT " ++ show x

instance Show Aexpr where
    show (NumExpr x) = show x
    show (VarExpr x) = show x
    show (SumExpr x y) = show x ++ " + " ++ show y
    show (ProdExpr x y) = show x ++ " * " ++ show y

instance Show Sexpr where
    show (LiteralExpr x) = "\"" ++ x ++ "\""
    show (ConcatExpr x y) = show x ++ "; " ++ show y
    show (ToStringExpr x) = show x

evalAexpr :: Aexpr -> Map Char Integer -> Maybe Integer
evalAexpr (NumExpr i) _ = Just i
evalAexpr (VarExpr c) m = Data.Map.Strict.lookup c m
evalAexpr (SumExpr a1 a2) m = (+) <$> (evalAexpr a1 m) <*> (evalAexpr a2 m)
evalAexpr (ProdExpr a1 a2) m = (*) <$> (evalAexpr a1 m) <*> (evalAexpr a2 m)

evalSexpr :: Sexpr -> Map Char Integer -> Maybe String
evalSexpr (LiteralExpr s) _ = Just s
evalSexpr (ConcatExpr s1 s2) m = (++) <$> (evalSexpr s1 m) <*> (evalSexpr s2 m)
evalSexpr (ToStringExpr a) m = show <$> evalAexpr a m 

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
