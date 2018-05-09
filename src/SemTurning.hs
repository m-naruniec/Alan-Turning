module SemTurning where

import AbsTurning
import ErrM
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

(!) = (M.!)
ident :: Ident -> String
ident (Ident v) = v

type Result = Err String
type PEnv = M.Map Ident Proc
type VEnv = M.Map Ident Loc
data Env = Env {pEnv :: PEnv, vEnv :: VEnv}

type Proc = [Loc] -> Trans Cont

type Store = M.Map Loc Val
type Loc = Integer
data Val = VInt Integer | VBool Bool
instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
(+?) :: Val -> Integer -> Val
(VInt i1) +? i2 = VInt (i1 + i2)
(-?) :: Val -> Integer -> Val
(VInt i1) -? i2 = VInt (i1 - i2)

newLocKey :: Loc
newLocKey = (-1)

getNewLoc :: Store -> Loc
getNewLoc s = newLoc where
  VInt newLoc = s ! newLocKey

setNewLoc :: Loc -> Store -> Store
setNewLoc loc s = M.insert newLocKey (VInt loc) s

postIncNewLoc :: Store -> (Loc, Store)
postIncNewLoc s = (newLoc, s') where
  newLoc = getNewLoc s
  s' = setNewLoc (newLoc + 1) s

alloc :: Val -> Store -> (Loc, Store)
alloc val s = (newLoc, s'') where
  (newLoc, s') = postIncNewLoc s
  s'' = M.insert newLoc val s'

type ContExp = Val -> Cont
type ContDecl = Env -> Cont
type Cont = Store -> Ans
type Ans = ExceptT String IO Store -- FIXME

type Trans a = a -> a -> (Cont, Cont)

type InterMonad a = Reader Env (Trans a)

interpret :: Prog -> IO ()
interpret (Prog stmt) = resultIO where
  resultMonad :: InterMonad Cont
  resultMonad = interStmt stmt

  resultIO :: IO ()
  resultIO = result where
    trans :: Trans Cont
    trans = (runReader resultMonad) startEnv
    resultCont :: Cont
    resultCont = snd $ trans startCont startCont
    resultExcept :: Ans
    resultExcept = (resultCont startStore) `catchError` (\str -> liftIO (putStrLn str >> return startStore))
    result = (runExceptT resultExcept) >> return ()

  startCont :: Cont
  startCont = \s -> return s

  startStore :: Store
  startStore = M.fromList [(newLocKey, VInt 0)]

  startEnv :: Env
  startEnv = Env M.empty M.empty

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

interStmt (SIfte e stmt1 stmt2) = do
  transExp <- interExp e
  trans1 <- interStmt stmt1
  trans2 <- interStmt stmt2
  let newTrans kl kr = transExp (fst . contExp) (snd . contExp) where
        contExp (VBool val) = if val then trans1 kl kr else trans2 kl kr
  return newTrans

interStmt (SIf e stmt1) = interStmt (SIfte e stmt1 SSkip)

interStmt (SBlock d stmt) = reader $ (\env kl kr ->
  let
    semDecl = runReader $ interDecl d
    semStmt = runReader $ interStmt stmt
    contDecl env' = semStmt env' kl kr
  in
    semDecl env (fst . contDecl) (snd . contDecl))

interStmt (SProc pIdent args) = do
  maybeProc <- asks (M.lookup pIdent . pEnv)
  case maybeProc of
    Nothing -> errMonad $ "Procedure " ++ show pIdent ++ " not in scope"
    Just proc ->

interStmt stmt = errMonad msg where
  msg = "Undefined yet: " ++ show stmt

interDecl :: Decl -> InterMonad ContDecl

-- todo refactor
interDecl (DProc pIdent params stmt) = reader $ (\(Env pEnv vEnv) klD krD ->
  let
    semStmt = runReader $ interStmt stmt
    resPEnv = M.insert pIdent (proc resEnv) pEnv
    resEnv = Env resPEnv vEnv
    proc (Env pEnv' vEnv') locs =
      let
        l = zip params locs
        vEnv'' = foldl f vEnv' l
        f vEnvArg (Param _ v, loc) = M.insert v loc vEnvArg
      in
        semStmt (Env pEnv' vEnv'')
  in
    (klD resEnv, krD resEnv))


interDecl (DDflt t v) = do
  Env pEnv vEnv <- ask
  let newTrans kl kr = (newCont kl, newCont kr) where
        newCont k s = k env' s' where
          (newLoc, s') = alloc (dflt t) s
          env' = Env pEnv (M.insert v newLoc vEnv)
  return newTrans

interDecl d = errMonad msg where
  msg = "Undefined yet: " ++ show d


interExp :: Exp -> InterMonad ContExp

interExp (EInt i) = expMonad $ VInt i

interExp (ETrue) = expMonad $ VBool True

interExp (EFalse) = expMonad $ VBool False

interExp (EVar v) = identOp id id v

interExp (EPreOp op v) = identOp f f v where
  f = case op of
    OpInc -> (+? 1)
    OpDec -> (-? 1)

interExp (EPostOp v op) = identOp id f v where
  f = case op of
    OpInc -> (+? 1)
    OpDec -> (-? 1)

interExp (EInv op e) = do
  transExp <- interExp e
  let newTrans kl kr = transExp (newCont kl) (newCont kr) where
        newCont k (VInt val) = k (VInt (-val))
        newCont k (VBool val) = k (VBool (not val))
  return newTrans

interExp e = errMonad msg where
  msg = "Undefined yet: " ++ show e

identOp :: (Val -> Val) -> (Val -> Val) -> Ident -> InterMonad ContExp
identOp resultChange storeChange v = do
  maybeLoc <- asks (M.lookup v . vEnv)
  case maybeLoc of
    Nothing -> errMonad $ "Variable " ++ (ident v) ++ " not in scope"
    Just loc -> let
        newTrans kl kr = (newCont kl, newCont kr)
        newCont k s = k res s' where
          val = s ! loc
          s' = M.insert loc (storeChange val) s
          res = resultChange val
      in
        return newTrans

expTrans :: Val -> Trans ContExp
expTrans v kl kr = (kl v, kr v)

expMonad :: Val -> InterMonad ContExp
expMonad v = return $ expTrans v

dflt :: Type -> Val
dflt TInt = VInt 0
dflt TBool = VBool False



errCont :: String -> Cont
errCont msg s = throwError msg

errTrans :: String -> Trans a
errTrans msg _ _ = (errCont msg, errCont msg)

errMonad :: String -> InterMonad a
errMonad msg = return $ errTrans msg



