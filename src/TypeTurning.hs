module TypeTurning where

import System.IO (stderr, hPutStrLn)
import AbsTurning
import ErrM
import PrintTurning
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M

(!) :: Ord a => M.Map a b -> a -> b
(!) = (M.!)
ident :: Ident -> String
ident (Ident v) = v

type PEnv = M.Map Ident Proc
type VEnv = M.Map Ident Type
data Env = Env {pEnv :: PEnv, vEnv :: VEnv}

type Proc = [Type]

type TypeMonad a = ReaderT Env (Except String) a

checkTypes :: Prog -> IO Bool
checkTypes (Prog stmt) = resultIO where
  eitherResult = runExcept $ runReaderT (checkStmt stmt) startEnv
  resultIO = case eitherResult of
    (Left str) -> hPutStrLn stderr str >> return False
    otherwise -> return True

  startEnv :: Env
  startEnv = Env M.empty M.empty


checkStmt :: Stmt -> TypeMonad ()

checkStmt (SSeq stmt1 stmt2) = do
  checkStmt stmt1
  checkStmt stmt2

checkStmt (SLeft stmt) = checkStmt stmt

checkStmt (SRight stmt) = checkStmt stmt

checkStmt (SPrint e) = checkExp_ e

checkStmt (SIfte be e1 e2) = do
  assert TBool be
  return ()

checkStmt (SIf be e1) = checkStmt (SIfte be e1 SSkip)

checkStmt (SExp e) = checkExp_ e

checkStmt stmt@(SProc pIdent args) = do
  params <- getProc pIdent
  let lArgs = length args
  let lParams = length params
  if lParams /= lArgs
    then argsErrMonad lParams lArgs stmt
    else
      let
        l = zip params $ toExp <$> args
        toExp (AVal e) = e
        toExp (ARef v) = EVar v
      in
        mapM_ (uncurry assert) l

checkStmt (SBlock d stmt) = do
  env' <- checkDecl d
  local (\_ -> env') $ checkStmt stmt

checkStmt stmt = return ()

checkDecl :: Decl -> TypeMonad Env

checkDecl (DSeq d1 d2) = do
  env' <- checkDecl d1
  local (\_ -> env') $ checkDecl d2

checkDecl (DDflt t v) = do
  env <- ask
  return $ setVar env v t

checkDecl (DVal t v e) = do
  assert t e
  checkDecl (DDflt t v)

checkDecl (DProc pIdent params stmt) = do
  env <- ask
  let params' = (\(Param t v) -> (v, t)) <$> params
  let params'' = snd <$> params'
  let setVar' env' (v, t) = setVar env' v t
  let addParams env' = foldl setVar' env' params'
  local addParams $ checkStmt stmt
  return $ setProc env pIdent params''

checkExp :: Exp -> TypeMonad Type

checkExp (EInt _) = return TInt

checkExp (ETrue) = return TBool

checkExp (EFalse) = return TBool

checkExp (EVar v) = getVar v

checkExp (EAss v op e) = do
  t <- getVar v
  assert t e
  when (op /= OpAss) $ assertEq TInt t e >> return ()
  return t

checkExp (EIfte be e1 e2) = do
  assert TBool be
  t <- checkExp e1
  assert t e2

checkExp (EOr e1 e2) = checkBin TBool e1 e2

checkExp (EAnd e1 e2) = checkBin TBool e1 e2

checkExp (EComp e1 _ e2) = do
  t <- checkExp e1
  assert t e2

checkExp (EOrd e1 _ e2) = checkBin TInt e1 e2

checkExp (EAdd e1 _ e2) = checkBin TInt e1 e2

checkExp (EMul e1 _ e2) = checkBin TInt e1 e2

checkExp (EInv OpNot e) = assert TBool e

checkExp (EInv OpNeg e) = assert TInt e

checkExp e@(EPreOp _ v) = do
  t <- getVar v
  assertEq TInt t e

checkExp e@(EPostOp v _) = do
  t <- getVar v
  assertEq TInt t e

checkExp_ :: Exp -> TypeMonad ()
checkExp_ e = checkExp e >> return ()

checkBin :: Type -> Exp -> Exp -> TypeMonad Type
checkBin t e1 e2 = do
  assert t e1
  assert t e2

assert :: Type -> Exp -> TypeMonad Type
assert expected e = do
  actual <- checkExp e
  assertEq expected actual e

assertEq :: Type -> Type -> Exp -> TypeMonad Type
assertEq expected actual e = do
  if actual == expected
    then return actual
    else typeErrMonad expected actual e

setVar :: Env -> Ident -> Type -> Env
setVar (Env pEnv vEnv) v t = Env pEnv $ M.insert v t vEnv

setProc :: Env -> Ident -> Proc -> Env
setProc (Env pEnv vEnv) pIdent proc = Env (M.insert pIdent proc pEnv) vEnv

getVar :: Ident -> TypeMonad Type
getVar v = do
  mType <- asks (M.lookup v . vEnv)
  case mType of
    Nothing -> varErrMonad v
    Just t -> return t

getProc :: Ident -> TypeMonad Proc
getProc pIdent = do
  mProc <- asks (M.lookup pIdent . pEnv)
  case mProc of
    Nothing -> procErrMonad pIdent
    Just proc -> return proc

errMonad :: String -> TypeMonad a
errMonad msg = throwError $ "[ERROR] " ++ msg

typeErrMonad :: Print a => Type -> Type -> a -> TypeMonad b
typeErrMonad expected actual e = errMonad $ unlines
  ["Type error:",
  "expected type: " ++ show expected,
  "actual type: " ++ show actual,
  "of expression: ",
  printTree e]

argsErrMonad :: Int -> Int -> Stmt -> TypeMonad a
argsErrMonad expected actual stmt = errMonad $ unlines
  ["Type error:",
  "expected " ++ show expected ++ " arguments, but received " ++ show expected,
  "in statement:",
  printTree stmt]

scopeErrMonad :: String -> TypeMonad a
scopeErrMonad msg = errMonad $ msg ++ " not in scope"

varErrMonad :: Ident -> TypeMonad a
varErrMonad v = scopeErrMonad $ "Variable " ++ ident v

procErrMonad :: Ident -> TypeMonad a
procErrMonad v = scopeErrMonad $ "Procedure " ++ ident v

