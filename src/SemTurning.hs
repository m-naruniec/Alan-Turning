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
data Val = VInt Int | VBool Bool

type ContExp = Val -> Cont
type ContDecl = Env -> Cont
type Cont = Store -> Ans
type Ans = IO ()

type InterMonad a = ReaderT Env (Except String) (a -> a -> (Cont, Cont))

interpret :: Prog -> IO ()
interpret (Prog stmt) = resultIO where
  resultMonad :: InterMonad Cont
  resultMonad = interStmt stmt `catchError` printError

  printError :: String -> InterMonad Cont
  printError error = return (\_ _ -> (printErrCont, printErrCont)) where
    printErrCont :: Cont
    printErrCont = \_ -> putStrLn error

  resultIO :: IO ()
  resultIO = resultCont startStore where
    Right trans = (runExcept $ (runReaderT resultMonad) startEnv)
    (_, resultCont) = trans startCont startCont

  startCont :: Cont
  startCont = \s -> return ()

  startStore :: Store
  startStore = empty

  startEnv :: Env
  startEnv = (empty, empty)

interStmt :: Stmt -> InterMonad Cont


interStmt stmt = throwError $ "Undefined yet: " ++ show stmt


failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProg :: Prog -> Result
transProg x = case x of
  Prog stmt -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  DVal type_ ident exp -> failure x
  DProc ident params decls stmt -> failure x
transParam :: Param -> Result
transParam x = case x of
  Param type_ ident -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x
  TBool -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  SSeq stmt1 stmt2 -> failure x
  SLeft stmt -> failure x
  SRight stmt -> failure x
  SSkip -> failure x
  SIf exp stmt -> failure x
  SIfte exp stmt1 stmt2 -> failure x
  STurn -> failure x
  SExp exp -> failure x
  SPrint exp -> failure x
  SBlock decls stmt -> failure x
  SProc ident args -> failure x
transArg :: Arg -> Result
transArg x = case x of
  AVal ident -> failure x
  ARef ident -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EAss ident exp -> failure x
  EAddAss ident exp -> failure x
  ESubAss ident exp -> failure x
  EMulAss ident exp -> failure x
  EDivAss ident exp -> failure x
  EModAss ident exp -> failure x
  EIfte exp1 exp2 exp3 -> failure x
  EOr exp1 exp2 -> failure x
  EAnd exp1 exp2 -> failure x
  EEq exp1 exp2 -> failure x
  ENEq exp1 exp2 -> failure x
  ELT exp1 exp2 -> failure x
  ELEq exp1 exp2 -> failure x
  EGT exp1 exp2 -> failure x
  EGEq exp1 exp2 -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EMod exp1 exp2 -> failure x
  ENot exp -> failure x
  ENeg exp -> failure x
  EInt integer -> failure x
  ETrue -> failure x
  EFalse -> failure x
  EPreInc ident -> failure x
  EPostInc ident -> failure x
  EPreDec ident -> failure x
  EPostDec ident -> failure x
  EVar ident -> failure x

