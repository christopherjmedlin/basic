-- prog represents the program as a map from a line number to the command at
-- that line number coupled with the number that follows it
data ProgEnv = {getProg :: Map Integer (Prog, Integer)}
-- valMap is a map from a symbol (e.g. X) to the value stored at that symbol
data ProgState = {getPC :: Integer, getValMap :: Map Char Integer}

type Exec = ReaderT ProgEnv (StateT ProgState (IO ()))

exec :: Com -> Exec
exec (LetCom s v) = do
    s <- get
    let vm = getValMap s
    put $ ProgState (getPC s) (insert s v vm) 
exec (PrintCom s) = do
    s <- get
    (liftIO . putStrLn) (eval s)

run :: Exec
run = do
    e <- ask
    s <- get
    let pc = getPC s
    let prog = getProg e
    let (c, next) = lookup pc prog
    exec c
    news <- get
    -- if the PC was not changed with a goto, then update PC to the next line
    if pc == (getPC news)
        then put $ ProgState next (getValMap news)
        else ()
    run 
