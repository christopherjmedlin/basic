module Expr
    ( someFunc
    ) where

data Com = LetCom | PrintCom
data Aexpr = NumExpr | SumExpr Aexpr Aexpr | Prod Aexpr Aexpr
