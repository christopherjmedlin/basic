module Exec where

import Data.Map.Strict as M
import Expr
import Control.Monad.State.Lazy
import Control.Monad.Reader
import System.Random hiding (next)
import Data.Time.Clock
import Data.Either
import Data.Foldable

type Exec = ReaderT ProgEnv (StateT ProgState IO)

tupMap f (x,y,z,k) = (f x, f y, f z, f k)
tupTrav f (x,y,z,k) = do {i1 <- f x; i2 <- f y; i3 <- f z; i4 <- f k; return (i1,i2,i3,i4)}

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

evalAndPutGen :: Aexpr -> Exec (Either String Number)
evalAndPutGen a = do
    s <- get
    let (i, g) = runEvalAexpr a s
    modify $ putGen g
    return i

--TODO a lot of these follow the pattern of evaluate arithmetic expression,
--then do something with it. can definitely be abstracted with a combinator
exec :: Com -> Exec ()
exec (LetCom (VarExpr c) v) = do
    s <- get
    let vm = getValMap s
    i <- evalAndPutGen v
    errorOrExec (\x -> modify (insertVal c x)) i

exec (LetCom (ArrExpr c ix) v) = do
    s <- get
    d <- tupTrav (fmap (toNormalInt . (fromRight (IntNum 0))) . evalAndPutGen) ix
    i <- evalAndPutGen v
    errorOrExec (\x -> modify (insertArr c d x)) i

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

exec (ForCom c (i, j, k)) = do
    s <- get
    res1 <- evalAndPutGen i
    res2 <- evalAndPutGen j
    res3 <- evalAndPutGen k
    let pc = getPC s
    errorOrExec (\x -> modify (insertVal c x)) res1
    let end = fromRight (IntNum pc) res2
    let step = fromRight (IntNum 1) res3
    modify $ insertIter c end pc step

exec (NextCom []) = return ()
exec (NextCom (c : cs)) = do
    s <- get
    let r = M.lookup c (getIters s)
    case r of
        Nothing -> execError ("No such iterator: " ++ [c])
        Just (i, j, k) -> do
            let val = maybe (IntNum 0) id (M.lookup c (getValMap s))
            if (addNums val k) <= i 
                then ((modify (incr c val k)) >> (modify (putPC j)))
                else exec (NextCom cs)
    where incr c i k = insertVal c (addNums i k)

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

exec (DimCom []) = return ()
exec (DimCom ((ArrExpr c dim) : xs)) = do
    s <- get
    d <- tupTrav (fmap (toNormalInt . (fromRight (IntNum 0))) . evalAndPutGen) dim
    modify $ newArr c d
    exec (DimCom xs)

exec RemCom = return ()

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
