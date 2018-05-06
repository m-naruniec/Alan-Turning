module SemTurning where

import AbsTurning
import ErrM
import Data.Map
import Control.Monad.Except
import Control.Monad.Reader

type Result = Err String
type PEnv = Map Ident Fun
type VEnv = Map Ident Loc
type Env = (PEnv, VEnv)

type Fun = [(Type, Ident)]

type Store = Map Loc Val
type Loc = Int
data Val = VInt Integer | VBool Bool
instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b

type ContExp = Val -> Cont
type ContDecl = Env -> Cont
type Cont = Store -> Ans
type Ans = ExceptT String IO ()

type Trans a = a -> a -> (Cont, Cont)

type InterMonad a = Reader Env (Trans a)

interpret :: Prog -> IO ()
interpret (Prog stmt) = resultIO where
--  resultMonad :: InterMonad Cont
--  resultMonad = interStmt stmt `catchError` printError
--
--  printError :: String -> InterMonad Cont
--  printError error = return (\_ _ -> (printErrCont, printErrCont)) where
--    printErrCont :: Cont
--    printErrCont = \_ -> putStrLn error
--
--  resultIO :: IO ()
--  resultIO = resultCont startStore where
--    Right trans = (runExcept $ (runReaderT resultMonad) startEnv)
--    (_, resultCont) = trans startCont startCont
  resultMonad :: InterMonad Cont
  resultMonad = interStmt stmt

  resultIO :: IO ()
  resultIO = result where
    trans :: Trans Cont
    trans = (runReader resultMonad) startEnv
    resultCont :: Cont
    resultCont = snd $ trans startCont startCont
    resultExcept :: Ans
    resultExcept = (resultCont startStore) `catchError` (\str -> liftIO (putStrLn str))
    result = (runExceptT resultExcept) >> return ()

  startCont :: Cont
  startCont = \s -> return ()

  startStore :: Store
  startStore = empty

  startEnv :: Env
  startEnv = (empty, empty)

interStmt :: Stmt -> InterMonad Cont

interStmt (SSkip) =
  return $ \kl kr -> (kl, kr)

interStmt (STurn) =
  return $ \kl kr -> (kr, kl)

interStmt (SLeft stmt) = do
  trans <- interStmt stmt
  let newTrans kl kr = (kl', kr) where
        (kl', _) = trans kl kr
  return newTrans

interStmt (SRight stmt) = do
  trans <- interStmt stmt
  let newTrans kl kr = (kl, kr') where
        (_, kr') = trans kl kr
  return newTrans

interStmt (SSeq stmt1 stmt2) = do
  trans1 <- interStmt stmt1
  trans2 <- interStmt stmt2
  let newTrans kl kr = (kl'', kr'') where
        (kl', kr'') = trans1 kl kr'
        (kl'', kr') = trans2 kl' kr
  return newTrans

interStmt (SPrint e) = do
  transExp <- interExp e
  let newTrans kl kr = transExp klExp krExp where
        f k v s = liftIO (putStrLn $ show v) >> k s
        klExp = f kl
        krExp = f kr
  return newTrans

interStmt (SExp e) = do
  transExp <- interExp e
  let newTrans kl kr = transExp (\_ -> kl) (\_ -> kr)
  return newTrans

interStmt stmt = errMonad msg where
  msg = "Undefined yet: " ++ show stmt


expTrans :: Val -> Trans ContExp
expTrans v kl kr = (kl v, kr v)

expMonad :: Val -> InterMonad ContExp
expMonad v = return $ expTrans v


interExp :: Exp -> InterMonad ContExp

interExp (EInt i) = expMonad $ VInt i

interExp (ETrue) = expMonad $ VBool True

interExp (EFalse) = expMonad $ VBool False

interExp e = errMonad msg where
  msg = "Undefined yet: " ++ show e



errCont :: String -> Cont
errCont msg s = throwError msg

errTrans :: String -> Trans a
errTrans msg _ _ = (errCont msg, errCont msg)

errMonad :: String -> InterMonad a
errMonad msg = return $ errTrans msg



