
module Types where



data Token
  = TModule
  | TImport
  | TExposing
  | TType
  | TAlias
  | TCase
  | TOf
  | TIf
  | TThen
  | TElse
  | TLet
  | TIn
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | TLParen
  | TRParen
  | TComma
  | TPipe
  | TDot
  | TEq
  | TArrow
  | TUnderscore
  | TBool Bool
  | TInt Int
  | TFloat Double
  | TChar Char
  | TString String
  | TOp String
  | TId String
  deriving (Show, Eq)

data ModuleDecl
  = ModuleDecl String [String] [Decl]
  deriving (Show, Eq)

data Decl
  = ImportDecl String [String]
  | TypeDecl String [String] [(String, [TypeExpr])]
  | TypeAliasDecl String [String] TypeExpr
  | AnnotationDecl String TypeExpr
  | ValueDecl String Expr
  deriving (Show, Eq)

data Expr
  = EVar String
  | EBool Bool
  | EInt Int
  | EFloat Double
  | EChar Char
  | EString String
  | EList [Expr]
  | EIf Expr Expr Expr
  | ECase Expr [(Pattern, Expr)]
  | ELet [LocalDef] Expr
  | EOp Expr String Expr
  | EApp Expr Expr
  deriving (Show, Eq)

data LocalDef
  = LDef String Expr
  deriving (Show, Eq)

data TypeExpr
  = TConErr String
  | TVarErr String
  | TRecordErr [(String, TypeExpr)]
  | TRecordTypeErr TypeExpr TypeExpr
  | TArrowErr TypeExpr TypeExpr
  deriving (Show, Eq)

data Pattern
  = PVar String
  | PLitInt Int
  | PLitBool Bool
  | PWildcard
  | PList [Pattern]
  | PNil
  deriving (Show, Eq)

parseError :: [Token] -> a
parseError _ = error "Parse error"
