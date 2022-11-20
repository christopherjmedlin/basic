module Expr (module Expr) where

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