module SemTurning where

import AbsTurning
import ErrM
import PrintTurning
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M

(!) = (M.!)
ident :: Ident -> String
ident (Ident v) = v

type Result = Err String
type PEnv = M.Map Ident Proc
type VEnv = M.Map Ident Loc
data Env = Env {pEnv :: PEnv, vEnv :: VEnv}

type Proc = [Loc] -> Trans ContPair

type Store = M.Map Loc Val
type Loc = Integer
data Val = VInt Integer | VBool Bool deriving (Eq, Ord)
instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b

(+?) :: Val -> Val -> Val
(VInt i1) +? (VInt i2) = VInt (i1 + i2)

(-?) :: Val -> Val -> Val
(VInt i1) -? (VInt i2) = VInt (i1 - i2)

(*?) :: Val -> Val -> Val
(VInt i1) *? (VInt i2) = VInt (i1 * i2)

(/?) :: Val -> Val -> Val
(VInt i1) /? (VInt i2) = VInt (i1 `div` i2)

(%?) :: Val -> Val -> Val
(VInt i1) %? (VInt i2) = VInt (i1 `mod` i2)

(&&?) :: Val -> Val -> Val
(VBool b1) &&? (VBool b2) = VBool (b1 && b2)

(||?) :: Val -> Val -> Val
(VBool b1) ||? (VBool b2) = VBool (b1 || b2)


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

type ContPairExp = Val -> ContPair
type ContPairDecl = Env -> ContPair
type ContPair = (Cont, Cont)
type Cont = Store -> Ans
type Ans = ExceptT String IO ()

type Trans a = a -> ContPair

type InterMonad a = Reader Env (Trans a)

-- TODO remove temporary test structures
interpret :: Prog -> IO ()
interpret (Prog stmt) = resultIO where
  resultMonad :: InterMonad ContPair
  resultMonad = interStmt stmt

  resultIO :: IO ()
  resultIO = result where
    trans :: Trans ContPair
    trans = (runReader resultMonad) testStartEnv
    resultCont :: Cont
    resultCont = snd $ trans (startCont, startCont)
    resultExcept :: Ans
    resultExcept = (resultCont testStartStore) `catchError` errorHandler
    errorHandler str = liftIO (putStrLn str)
    result = (runExceptT resultExcept) >> return ()

  startCont :: Cont
  startCont _ = return ()

  startStore :: Store
  startStore = M.fromList [(newLocKey, VInt 0)]

  startEnv :: Env
  startEnv = Env M.empty M.empty

  testStartStore :: Store
  testStartStore = M.insert (-2) (VInt 42) startStore

  testStartEnv :: Env
  testStartEnv = Env (pEnv startEnv) (M.insert (Ident "test") (-2) $ vEnv startEnv)

interStmt :: Stmt -> InterMonad ContPair

interStmt (SSkip) = return id

interStmt (STurn) = return swap

interStmt (SLeft stmt) = do
  trans <- interStmt stmt
  let newTrans (kl, kr) = (kl', kr) where
        (kl', _) = trans (kl, kr)
  return newTrans

interStmt (SRight stmt) = do
  trans <- interStmt stmt
  let newTrans (kl, kr) = (kl, kr') where
        (_, kr') = trans (kl, kr)
  return newTrans

interStmt (SSeq stmt1 stmt2) = do
  trans1 <- interStmt stmt1
  trans2 <- interStmt stmt2
  let newTrans (kl, kr) = (kl'', kr'') where
        (kl', kr'') = trans1 (kl, kr')
        (kl'', kr') = trans2 (kl', kr)
  return newTrans

interStmt (SPrint e) = do
  transExp <- interExp e
  let newTrans (kl, kr) = transExp kpExp where
        kpExp val = (newCont kl, newCont kr) where
          newCont k s = liftIO (putStrLn $ show val) >> k s
  return newTrans

interStmt (SExp e) = do
  transExp <- interExp e
  return $ (\kp -> transExp (\_ -> kp))

interStmt (SIfte e stmt1 stmt2) = do
  transExp <- interExp e
  trans1 <- interStmt stmt1
  trans2 <- interStmt stmt2
  let newTrans kp = transExp kExp where
        kExp (VBool val) = if val then trans1 kp else trans2 kp
  return newTrans

interStmt (SIf e stmt1) = interStmt (SIfte e stmt1 SSkip)

interStmt (SBlock d stmt) = reader $ \env kp ->
  let
    semDecl = runReader $ interDecl d
    semStmt = runReader $ interStmt stmt
  in
    semDecl env $ \env' -> semStmt env' kp

interStmt (SProc pIdent args) = do
  transArgs <- interArgs args
  vEnv <- asks (vEnv)
  maybeProc <- asks (M.lookup pIdent . pEnv)
  case maybeProc of
    Nothing -> procErrMonad pIdent
    Just proc ->
        let
          newTrans :: Trans ContPair
          newTrans kp = transArgs (\locs -> (newCont fst locs, newCont snd locs)) where
            newCont selector locs = selector $ proc locs kp
        in return newTrans

interArgs :: [Arg] -> InterMonad ([Loc] -> ContPair)
interArgs [] = return (\kpLocs -> kpLocs [])

interArgs ((ARef v):t) = do
  transArgs <- interArgs t
  mLoc <- asks $ M.lookup v . vEnv
  case mLoc of
    Nothing -> varErrMonad v
    Just loc ->
      let
        newTrans kpLocs = transArgs (\locs -> kpLocs (loc:locs))
      in
        return newTrans

interArgs ((AVal e):t) = do
  transArgs <- interArgs t
  transExp <- interExp e
  let newTrans kpLocs = transExp (\val ->
        let
          contArgs locs = (newCont fst, newCont snd) where
            newCont selector s = selector (kpLocs (loc:locs)) s' where
              (loc, s') = alloc val s
        in
          transArgs contArgs)
  return newTrans

interDecl :: Decl -> InterMonad ContPairDecl

interDecl (DProc pIdent params stmt) = reader $ \(Env pEnv vEnv) kpD ->
  let
    semStmt = runReader $ interStmt stmt
    resPEnv = M.insert pIdent proc pEnv
    resEnv = Env resPEnv vEnv
    proc locs =
      let
        l = zip params locs
        vEnv' = foldl f vEnv l
        f vEnvAcc (Param _ v, loc) = M.insert v loc vEnvAcc
      in
        semStmt (Env resPEnv vEnv')
  in
    kpD resEnv

interDecl (DDflt t v) = interDecl $ DVal t v (dfltExp t)

interDecl (DVal _ v e) = do
  transExp <- interExp e
  Env pEnv vEnv <- ask
  let newTrans kpD = transExp contExp where
        contExp val = (newCont fst, newCont snd) where
          newCont selector s = selector (kpD env') s' where
            (newLoc, s') = alloc val s
            env' = Env pEnv (M.insert v newLoc vEnv)
  return newTrans

interDecl (DSeq d1 d2) = reader $ \env kp ->
  let
    semDecl1 = runReader $ interDecl d1
    semDecl2 = runReader $ interDecl d2
  in
    semDecl1 env (\env' -> semDecl2 env' kp)

one :: Val
one = VInt 1

zero :: Val
zero = VInt 0

valPred :: (Val -> Val -> Bool) -> (Val -> Val -> Val)
valPred f = curry $ VBool . (uncurry f)

interExp :: Exp -> InterMonad ContPairExp

interExp (EInt i) = return $ (\kp -> kp $ VInt i)

interExp (ETrue) = return $ (\kp -> kp $ VBool True)

interExp (EFalse) = return $ (\kp -> kp $ VBool False)

interExp (EVar v) = identExp id id v

interExp (EPreOp op v) = identExp f f v where
  f = case op of
    OpInc -> (+? one)
    OpDec -> (-? one)

interExp (EPostOp v op) = identExp id f v where
  f = case op of
    OpInc -> (+? one)
    OpDec -> (-? one)

interExp e@(EAdd e1 op e2) = binOpExp False f e e1 e2 where
  f = case op of
    OpPlus -> (+?)
    OpMinus -> (-?)

interExp e@(EMul e1 OpTimes e2) = binOpExp False (*?) e e1 e2

interExp e@(EMul e1 op e2) = binOpExp True f e e1 e2 where
  f = case op of
    OpDiv -> (/?)
    OpMod -> (%?)

interExp e@(EOr e1 e2) = binOpExp False (||?) e e1 e2

interExp e@(EAnd e1 e2) = binOpExp False (&&?) e e1 e2

interExp e@(EComp e1 op e2) = binOpExp False (valPred f) e e1 e2 where
  f = case op of
    OpEq -> (==)
    OpNEq -> (/=)

interExp e@(EOrd e1 op e2) = binOpExp False (valPred f) e e1 e2 where
  f = case op of
    OpLT -> (<)
    OpLEq -> (<=)
    OpGT -> (>)
    OpGEq -> (>=)

interExp context@(EAss v op e) = do
  transExp <- interExp e
  mLoc <- asks (M.lookup v . vEnv)
  case mLoc of
    Nothing -> varErrMonad v
    Just loc ->
      let
        newTrans kpExp = transExp contExp where
          (f, check) = case op of
            OpAss -> ((\_ val -> val), False)
            OpAddAss -> ((+?), False)
            OpSubAss -> ((-?), False)
            OpMulAss -> ((*?), False)
            OpDivAss -> ((/?), True)
            OpModAss -> ((%?), True)
          contExp rightVal = if (check && rightVal == zero)
            then divErrPair context
            else (newCont fst, newCont snd) where
              newCont selector s = selector (kpExp newVal) s' where
                oldVal = (s ! loc)
                newVal = f oldVal rightVal
                s' = M.insert loc newVal s
      in
        return newTrans

interExp (EIfte be e1 e2) = do
  transBExp <- interExp be
  transExp1 <- interExp e1
  transExp2 <- interExp e2
  return $ \kpE -> transBExp (\(VBool b) ->
    if b then transExp1 kpE else transExp2 kpE)


interExp (EInv op e) = do
  transExp <- interExp e
  let newTrans kp = transExp newCont where
        newCont (VInt val) = kp (VInt (-val))
        newCont (VBool val) = kp (VBool (not val))
  return newTrans


binOpExp :: Bool -> (Val -> Val -> Val) -> Exp -> Exp -> Exp -> InterMonad ContPairExp
binOpExp check f contextExp e1 e2 = do
  transExp1 <- interExp e1
  transExp2 <- interExp e2
  return $ (\kpE -> transExp1 (\val1 -> transExp2 (\val2 ->
    if (check && val2 == zero)
      then divErrPair contextExp
      else kpE (f val1 val2))))

identExp :: (Val -> Val) -> (Val -> Val) -> Ident -> InterMonad ContPairExp
identExp resultChange storeChange v = do
  mLoc <- asks (M.lookup v . vEnv)
  case mLoc of
    Nothing -> varErrMonad v
    Just loc -> let
        newTrans kp = (newCont fst, newCont snd) where
          newCont selector s = selector (kp res) s' where
            val = s ! loc
            s' = M.insert loc (storeChange val) s
            res = resultChange val
      in
        return newTrans

dfltExp :: Type -> Exp
dfltExp TInt = EInt 0
dfltExp TBool = EFalse


errCont :: String -> Cont
errCont msg s = throwError $ "[ERROR] " ++ msg

errTrans :: String -> Trans a
errTrans msg _ = (errCont msg, errCont msg)

errMonad :: String -> InterMonad a
errMonad msg = return $ errTrans msg

divErrPair :: Exp -> (Cont, Cont)
divErrPair context =
  errTrans ("Division by zero in " ++ printTree context) $ errCont ""

scopeErrMonad :: String -> InterMonad a
scopeErrMonad str = errMonad $ str ++ " not in scope"

varErrMonad :: Ident -> InterMonad a
varErrMonad v = scopeErrMonad $ "Variable " ++ ident v

procErrMonad :: Ident -> InterMonad a
procErrMonad p = scopeErrMonad $ "Procedure " ++ ident p

