module Exec where

import Data.Map.Strict as M
import Expr
import Control.Monad.State.Lazy
import Control.Monad.Reader
import System.Random
import Data.Time.Clock

type Exec = ReaderT ProgEnv (StateT ProgState IO)

terminate :: Exec ()
terminate = put $ ProgState (-1) empty (mkStdGen 0) empty [] False empty

execError :: String -> Exec ()
execError str = do
    s <- get
    (liftIO . putStrLn) $ "Error at " ++ (show (getPC s)) ++ ": " ++ str
    terminate
    
errorOrExec :: (a -> Exec ()) -> Either String a -> Exec ()
errorOrExec f (Left str) = execError str
errorOrExec f (Right val) = f val

goto :: Integer -> Exec ()
goto i = modify (putPC i)

gotoAndSet :: Integer -> Exec ()
gotoAndSet i = goto i >> modify setGoto

exec :: Com -> Exec ()
exec (LetCom c v) = do
    s <- get
    let vm = getValMap s
    let i = evalAexpr v s
    errorOrExec (\x -> modify (insertVal c x)) i

exec (PrintCom (NoNewLineExpr str)) = do
    s <- get
    let res = evalSexpr str s
    errorOrExec (\x -> (liftIO . putStr) x) res

exec (PrintCom str) = do
    s <- get
    let res = evalSexpr str s
    errorOrExec (\x -> (liftIO . putStrLn) x) res

exec EndCom = terminate

exec (GotoCom i) = gotoAndSet i

exec (IfCom b i) = do
    s <- get
    let res = evalBexpr b s
    errorOrExec (\x -> if x then gotoAndSet i else return ()) res

exec (ForCom c (i, j)) = do
    s <- get
    let res1 = evalAexprInt i s
    let res2 = evalAexprInt j s
    let pc = getPC s
    errorOrExec (\x -> modify (insertVal c x)) res1
    errorOrExec (\x -> modify (insertIter c x pc)) res2

exec (NextCom c) = do
    s <- get
    let r = M.lookup c (getIters s)
    case r of
        Nothing -> execError ("No such iterator: " ++ [c])
        Just (i, j) -> do
            let val = maybe (IntNum 0) id (M.lookup c (getValMap s))
            if val < i 
                then ((modify (incr c val)) >> (modify (putPC j)))
                else return ()
    where incr c i = insertVal c (addNums i (IntNum 1))

exec (InputCom s c) = do
    (liftIO . putStrLn) s
    s <- liftIO getLine
    modify (insertVal c ((IntNum . read) s)) -- TODO floats          

exec (GoSubCom i) = do
    s <- get
    modify (pushStack (getPC s))
    gotoAndSet i

exec ReturnCom = do
    s <- get
    -- do not set gotoflag when returning, else we just call the routine again
    goto (peekStack s)
    modify popStack

exec (SeqCom c1 c2) = exec c1 >> exec c2

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
    -- get the new PC after modification
    let (c, next) = case M.lookup (getPC news) prog of
            Just (com, nextl) -> (com, nextl)
            Nothing           -> ((PrintCom (LiteralExpr "Error")), -1)
    -- if the gotoflag is not set
    if (not (gotoFlag news))
        then modify (putPC next)
        else return ()
    modify unsetGoto
    quitIfFinished

initGen :: Exec ()
initGen = do
    time <- liftIO getCurrentTime
    s <- get
    let g = (mkStdGen.fromIntegral) (diffTimeToPicoseconds (utctDayTime time))
    modify (putGen g)

start :: Map Integer (Com, Integer) -> Integer -> IO ()
start prog init = evalStateT (runReaderT (initGen >> run) env) state
    where
        env   = ProgEnv prog
        state = ProgState init M.empty (mkStdGen 0) M.empty [] False empty
