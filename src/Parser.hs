module Parser (module Parser) where

import Text.Parsec
import Text.Parsec.String
import Expr
import Control.Monad
import Data.Char
import Data.Map
import Data.List (sort, sortOn)

consumeUntil :: Char -> Parser String
consumeUntil u = do
    c <- anyChar
    if c == u 
        then 
            return [] 
        else do
            x <- consumeUntil u
            return (c : x)

integer :: Parser Integer
integer = read <$> many1 digit

float :: Parser Double
float = read <$> do
    d1 <- many1 digit
    c <- char '.'
    d2 <- many1 digit
    return $ d1 ++ [c] ++ d2

anyString :: Parser String
anyString = many1 anyChar

numExpr :: Parser Aexpr
numExpr = NumExpr <$> integer

floatExpr :: Parser Aexpr
floatExpr = FloatExpr <$> float

anyAlpha :: Parser Char
anyAlpha = do
    c <- anyChar
    guard (isAlpha c)
    return c

varExpr :: Parser Aexpr
varExpr = VarExpr <$> anyAlpha

sumExpr :: Parser Aexpr
sumExpr = do
    a1 <- try prodExpr <|> try aexprParens <|> try floatExpr <|> numExpr 
          <|> try varExpr
    spaces
    char '+'
    spaces
    a2 <- aexpr
    return $ SumExpr a1 a2

prodExpr :: Parser Aexpr
prodExpr = do
    a1 <- try func <|> try aexprParens <|> try floatExpr <|> numExpr <|> try varExpr
    spaces
    char '*'
    spaces
    a2 <- try func <|> try prodExpr <|> try aexprParens <|> try floatExpr <|> numExpr 
          <|> varExpr
    return $ ProdExpr a1 a2

aexprParens :: Parser Aexpr
aexprParens = do
    char '('
    spaces
    a <- aexpr
    spaces
    char ')'
    return a

funcs = string "INT" <|> string "RND"

nameToFunc :: String -> (Aexpr -> Aexpr)
nameToFunc "INT" = IntExpr
nameToFunc "RND" = RndExpr

func :: Parser Aexpr
func = do
    name <- funcs
    aexpr <- aexprParens
    return $ nameToFunc name aexpr

aexpr :: Parser Aexpr
aexpr = try sumExpr <|> try prodExpr <|> try func <|> try floatExpr 
        <|> try numExpr <|> try varExpr <|> try aexprParens

toStringExpr :: Parser Sexpr
toStringExpr = do
    a <- aexpr
    return $ ToStringExpr a

literalExpr :: Parser Sexpr
literalExpr = LiteralExpr <$> (char '"' >> consumeUntil '"')

concatExpr :: Parser Sexpr
concatExpr = ConcatExpr <$> left <*> sexpr
    where left = do
            s <- toStringExpr <|> literalExpr
            char ';'
            spaces
            return s

sexpr :: Parser Sexpr
sexpr = try concatExpr <|> toStringExpr <|> literalExpr

charToComp :: Char -> (Aexpr -> Aexpr -> Bexpr)
charToComp '=' = EqExpr
charToComp '>' = GeExpr
charToComp '<' = LeExpr

bexpr :: Parser Bexpr
bexpr = do
    a1 <- aexpr
    spaces
    c <- char '=' <|> char '<' <|> char '>'
    spaces
    a2 <- aexpr
    return $ (charToComp c) a1 a2

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

ifCom :: Parser Com
ifCom = do
    string "IF"
    spaces
    b <- bexpr
    spaces
    string "THEN"
    spaces
    i <- integer
    return $ IfCom b i

forCom :: Parser Com
forCom = do
    string "FOR"
    spaces
    c <- anyChar
    spaces
    char '='
    spaces
    a1 <- aexpr
    spaces
    string "TO"
    spaces
    a2 <- aexpr
    return $ ForCom c (a1, a2)

nextCom :: Parser Com
nextCom = do
    string "NEXT"
    spaces
    c <- anyChar
    return $ NextCom c

inputCom :: Parser Com
inputCom = do
    string "INPUT"
    spaces
    s <- sexpr
    case s of
        (ConcatExpr (LiteralExpr s) (ToStringExpr (VarExpr c))) -> return $ InputCom s c
        (ToStringExpr (VarExpr c)) -> return $ InputCom "" c
        _ -> fail "Invalid input command"

com :: Parser Com
com = printCom <|> letCom <|> endCom <|> gotoCom <|> try ifCom <|> forCom <|> nextCom <|> inputCom

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
