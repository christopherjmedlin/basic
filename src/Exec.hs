module Exec where

import Data.Map.Strict as M
import Expr
import Control.Monad.State.Lazy
import Control.Monad.Reader

-- prog represents the program as a map from a line number to the command at
-- that line number coupled with the number that follows it
data ProgEnv = ProgEnv {getProg :: Map Integer (Com, Integer)}
-- valMap is a map from a symbol (e.g. X) to the value stored at that symbol
data ProgState = ProgState {getPC :: Integer, getValMap :: Map Char Integer}

type Exec = ReaderT ProgEnv (StateT ProgState IO)

exec :: Com -> Exec ()
exec (LetCom c v) = do
    s <- get
    let vm = getValMap s
    let i = evalAexpr v vm
    case i of
        Just result -> put $ ProgState (getPC s) (insert c result vm) 
        Nothing     -> (liftIO . putStrLn) "No such variable"
exec (PrintCom str) = do
    s <- get
    let res = evalSexpr str (getValMap s)
    case res of
        Just result -> (liftIO . putStrLn) result
        Nothing     -> (liftIO . putStrLn) "No such variable"
exec EndCom = put $ ProgState (-1) empty

quitIfFinished :: Exec ()
quitIfFinished = do
    s <- get
    if (getPC s) == -1
        then return ()
        else run

run :: Exec ()
run = do
    e <- ask
    s <- get
    let pc = getPC s
    let prog = getProg e
    let (c, next) = case M.lookup pc prog of
            Just (com, nextl) -> (com, nextl)
            Nothing           -> ((PrintCom (LiteralExpr "Error")), -1)
    exec c
    news <- get
    -- if the PC was not changed with a goto, then update PC to the next line
    if pc == (getPC news)
        then put $ ProgState next (getValMap news)
        else return ()
    quitIfFinished

start :: Map Integer (Com, Integer) -> Integer -> IO ()
start prog init = evalStateT (runReaderT run env) state
    where
        env   = ProgEnv prog
        state = ProgState init M.empty
