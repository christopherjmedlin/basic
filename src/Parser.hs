module Parser (module Parser) where

import Text.Parsec
import Text.Parsec.String
import Expr
import Control.Monad
import Data.Char
import Data.Map
import Data.List (sort, sortOn)

integer :: Parser Integer
integer = read <$> many1 digit

anyString :: Parser String
anyString = many1 anyChar

numExpr :: Parser Aexpr
numExpr = NumExpr <$> integer

anyAlpha :: Parser Char
anyAlpha = do
    c <- anyChar
    guard (isAlpha c)
    return c

varExpr :: Parser Aexpr
varExpr = VarExpr <$> anyAlpha

sumExpr :: Parser Aexpr
sumExpr = do
    a1 <- try prodExpr <|> try aexprParens <|> numExpr <|> try varExpr
    spaces
    char '+'
    spaces
    a2 <- aexpr
    return $ SumExpr a1 a2

prodExpr :: Parser Aexpr
prodExpr = do
    a1 <- try aexprParens <|> numExpr <|> try varExpr
    spaces
    char '*'
    spaces
    a2 <- try prodExpr <|> try aexprParens <|> numExpr <|> varExpr
    return $ ProdExpr a1 a2

aexprParens :: Parser Aexpr
aexprParens = do
    char '('
    spaces
    a <- aexpr
    spaces
    char ')'
    return a

aexpr :: Parser Aexpr
aexpr = try sumExpr <|> try prodExpr <|> try numExpr <|> try varExpr <|> try aexprParens

toStringExpr :: Parser Sexpr
toStringExpr = do
    a <- aexpr
    return $ ToStringExpr a

sexpr :: Parser Sexpr
sexpr = toStringExpr

letCom :: Parser Com
letCom = do
    string "LET"
    spaces
    c <- anyChar
    spaces
    char '='
    spaces
    e <- aexpr
    return $ LetCom c e

printCom :: Parser Com
printCom = do
    string "PRINT"
    spaces
    s <- sexpr
    return $ PrintCom s

endCom :: Parser Com
endCom = do {string "END"; return EndCom}

gotoCom :: Parser Com
gotoCom = do
    string "GOTO"
    spaces
    i <- integer
    return $ GotoCom i

com :: Parser Com
com = printCom <|> letCom <|> endCom <|> gotoCom

line :: Parser (Integer, Com)
line = do 
    i <- integer
    spaces
    c <- com
    -- spaces wil consume newline, don't use
    (many (char ' ')) 
    ((const ()) <$> char '\n') <|> eof
    return (i, c)

lines = many1 line

parseLines :: String -> Either ParseError [(Integer, (Com, Integer))]
parseLines = (fmap (addNextLines . (sortOn fst))) . parse
    where parse = runParser Parser.lines () ""

-- after interpreting a command, the interpreter can then get the number of the
-- command immediately following it to increment the PC (if a goto didn't
-- happen)
addNextLines :: [(Integer, Com)] -> [(Integer, (Com, Integer))]
addNextLines [] = []
addNextLines [(i, c)] = [(i, (c, -1))]
addNextLines ((i, c) : xs) = (i, (c, i2)) : addNextLines xs
    where (i2, c2) = head xs

-- returns the program and the number of the first instruction
parseProgram :: String -> Either ParseError (Map Integer (Com, Integer), Integer)
parseProgram str = fmap go (parseLines str)
    where go ls = (fromList ls, (fst.head) ls)
