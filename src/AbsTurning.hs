

module AbsTurning where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Prog = Prog Stmt
  deriving (Eq, Ord, Show, Read)

data Decl
    = DSeq Decl Decl
    | DDflt Type Ident
    | DVal Type Ident Exp
    | DProc Ident [Param] Stmt
  deriving (Eq, Ord, Show, Read)

data Param = Param Type Ident
  deriving (Eq, Ord, Show, Read)

data Type = TInt | TBool
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SSeq Stmt Stmt
    | SLeft Stmt
    | SRight Stmt
    | SSkip
    | SIf Exp Stmt
    | SIfte Exp Stmt Stmt
    | STurn
    | SExp Exp
    | SPrint Exp
    | SBlock Decl Stmt
    | SProc Ident [Arg]
  deriving (Eq, Ord, Show, Read)

data Arg = AVal Exp | ARef Ident
  deriving (Eq, Ord, Show, Read)

data Exp
    = EAss Ident OpAss Exp
    | EIfte Exp Exp Exp
    | EOr Exp Exp
    | EAnd Exp Exp
    | EComp Exp OpComp Exp
    | EOrd Exp OpOrd Exp
    | EAdd Exp OpAdd Exp
    | EMul Exp OpMul Exp
    | EInv OpInv Exp
    | EVar Ident
    | EInt Integer
    | ETrue
    | EFalse
    | EPreOp OpPrePost Ident
    | EPostOp Ident OpPrePost
  deriving (Eq, Ord, Show, Read)

data OpAss
    = OpAss | OpAddAss | OpSubAss | OpMulAss | OpDivAss | OpModAss
  deriving (Eq, Ord, Show, Read)

data OpComp = OpEq | OpNEq
  deriving (Eq, Ord, Show, Read)

data OpOrd = OpLT | OpLEq | OpGT | OpGEq
  deriving (Eq, Ord, Show, Read)

data OpAdd = OpPlus | OpMinus
  deriving (Eq, Ord, Show, Read)

data OpMul = OpTimes | OpDiv | OpMod
  deriving (Eq, Ord, Show, Read)

data OpInv = OpNot | OpNeg
  deriving (Eq, Ord, Show, Read)

data OpPrePost = OpInc | OpDec
  deriving (Eq, Ord, Show, Read)
