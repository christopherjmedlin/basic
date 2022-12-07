module Main where

import Text.Parsec
import Expr
import Parser
import Exec

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if (null args)
        then error "Usage: based <filename>"
        else return ()
    s <- readFile (head args)
    let result = parseProgram s
    case result of
        (Left err) -> error (show err)
        (Right p)  -> (uncurry start) p
