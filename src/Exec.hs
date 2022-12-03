module Exec where

import Data.Map.Strict as M
import Expr
import Control.Monad.State.Lazy
import Control.Monad.Reader
import System.Random
import Data.Time.Clock

type Exec = ReaderT ProgEnv (StateT ProgState IO)

terminate :: Exec ()
terminate = put $ ProgState (-1) empty (mkStdGen 0)

errorOrExec :: (a -> Exec ()) -> Either String a -> Exec ()
errorOrExec f (Left str) = do
    s <- get
    (liftIO . putStrLn) $ "Error at " ++ (show (getPC s)) ++ ": " ++ str
    terminate
errorOrExec f (Right val) = f val

goto :: Integer -> Exec ()
goto i = do
    s <- get
    put $ ProgState i (getValMap s) (getGen s)

exec :: Com -> Exec ()
exec (LetCom c v) = do
    s <- get
    let vm = getValMap s
    let i = evalAexpr v s
    errorOrExec (\x -> put $ ProgState (getPC s) (insert c x vm) (getGen s)) i
exec (PrintCom str) = do
    s <- get
    let res = evalSexpr str s
    errorOrExec (\x -> (liftIO . putStrLn) x) res
exec EndCom = terminate
exec (GotoCom i) = goto i
exec (IfCom b i) = do
    s <- get
    let res = evalBexpr b s
    errorOrExec (\x -> if x then goto i else return ()) res

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
        then put $ ProgState next (getValMap news) (getGen news)
        else return ()
    quitIfFinished

initGen :: Exec ()
initGen = do
    time <- liftIO getCurrentTime
    s <- get
    let g = (mkStdGen.fromIntegral) (diffTimeToPicoseconds (utctDayTime time))
    put $ ProgState (getPC s) (getValMap s) g

start :: Map Integer (Com, Integer) -> Integer -> IO ()
start prog init = evalStateT (runReaderT (initGen >> run) env) state
    where
        env   = ProgEnv prog
        state = ProgState init M.empty (mkStdGen 0)
