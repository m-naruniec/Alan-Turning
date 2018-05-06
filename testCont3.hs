import Control.Monad.Reader
import Data.Map

type Env = Map String Int
type Store = Map String Int
type Cont = Store -> Ans
type Ans = IO ()

type MyMonad = Cont -> Cont -> (Cont, Cont)

data Stmt = SPrint String
    | SLeft Stmt
    | SRight Stmt
    | STurn
    | SSeq Stmt Stmt
    | SInc String

execStmt' :: Stmt -> MyMonad

execStmt' (SInc v) kl kr = (kl', kr') where
    trans k s = k (insert v (val + 1) s) where
        val = s ! v
    kl' = trans kl
    kr' = trans kr

execStmt' (SPrint v) kl kr = (kl', kr') where
    trans k s = (putStrLn str) >> k s where
        str = show (s ! v)
    kl' = trans kl
    kr' = trans kr

execStmt' (SLeft stmt) kl kr = (kl', kr) where
    (kl', _) = execStmt' stmt kl kr

execStmt' (SRight stmt) kl kr = (kl, kr') where
    (_, kr') = execStmt' stmt kl kr

execStmt' (SSeq stmt1 stmt2) kl kr = (kl'', kr'') where
    (kl', kr'') = execStmt' stmt1 kl kr'
    (kl'', kr') = execStmt' stmt2 kl' kr

execStmt' (STurn) kl kr = (kr, kl)

testStmt :: Stmt
testStmt = --SSeq (SInc "x") (SPrint "x")
    (SSeq (SInc "x")
    (SSeq (SLeft (SPrint "x"))
    (SSeq (SInc "x")
    (SSeq (SRight (SPrint "x"))
    (SSeq (SLeft STurn)
    (SSeq (SPrint "x")
    (SSeq (SInc "x")
    (SSeq (SRight STurn)
    (SSeq (SLeft (SPrint "x"))
    (SRight (SPrint "x")))))))))))
runTest :: (IO (), IO ())
runTest = (kl startStore, kr startStore) where
    (kl, kr) = (execStmt' testStmt startCont startCont)

startStore :: Store
startStore = fromList [("x", 0)]
startCont :: Cont
startCont = \_ -> return ()

