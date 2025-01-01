
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser where


import Data.Char
import Data.List
import Data.Array
import Control.Monad
import System.Exit
import System.Environment (getProgName)
import Data.List (isPrefixOf)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

import Types ( Token(..), ModuleDecl(..), Decl(..), Expr(..), LocalDef(..), TypeExpr(..), Pattern(..), parseError)

}

%name parseElm Module

%tokentype { Token }

%error { parseError }

%token
  TModule         { TModule }
  TImport         { TImport }
  TExposing       { TExposing }
  TType           { TType }
  TAlias          { TAlias }
  TCase           { TCase }
  TOf             { TOf }
  TIf             { TIf }
  TThen           { TThen }
  TElse           { TElse }
  TLet            { TLet }
  TIn             { TIn }
  TLBrace         { TLBrace }
  TRBrace         { TRBrace }
  TLBracket       { TLBracket }
  TRBracket       { TRBracket }
  TLParen         { TLParen }
  TRParen         { TRParen }
  TComma          { TComma }
  TPipe           { TPipe }
  TDot            { TDot }
  TEq             { TEq }
  TArrow          { TArrow }
  TUnderscore     { TUnderscore }
  TBool           { TBool $$ }
  TInt            { TInt $$ }
  TFloat          { TFloat $$ }
  TChar           { TChar $$ }
  TString         { TString $$ }
  TOp             { TOp $$ }
  TId             { TId $$ }

%%

Module :: { ModuleDecl }
  : TModule TId TExposing TLParen ExposeList TRParen DeclList
    { ModuleDecl $2 $5 $7 }

ExposeList :: { [String] }
  : TId
    { [$1] }
  | TId TComma ExposeList
    { $1 : $3 }
  |
    { [] }

DeclList :: { [Decl] }
  : Decl DeclList
    { $1 : $2 }
  |
    { [] }

Decl :: { Decl }
  : ImportDecl
    { $1 }
  | TypeDeclaration
    { $1 }
  | TypeAliasDeclaration
    { $1 }
  | AnnotationDeclaration
    { $1 }
  | ValueDeclaration
    { $1 }

ImportDecl :: { Decl }
  : TImport TId MaybeExposing
    { ImportDecl $2 $3 }

MaybeExposing :: { [String] }
  : TExposing TLParen ExposeList TRParen
    { $3 }
  |
    { [] }

TypeDeclaration :: { Decl }
  : TType TId TypeVars TEq TypeCtors
    { TypeDecl $2 $3 $5 }

TypeAliasDeclaration :: { Decl }
  : TType TAlias TId TypeVars TEq TypeExpr
    { TypeAliasDecl $3 $4 $6 }

TypeCtors :: { [(String, [TypeExpr])] }
  : TypeCtor
    { [$1] }
  | TypeCtor TPipe TypeCtors
    { $1 : $3 }

TypeCtor :: { (String, [TypeExpr]) }
  : TId TypeExprList
    { ($1, $2) }

TypeExprList :: { [TypeExpr] }
  :
    { [] }
  | TypeExpr TypeExprList
    { $1 : $2 }

TypeExpr :: { TypeExpr }
  : TId
    { TConErr $1 }
  | TLParen TypeExpr TRParen
    { $2 }
  | TLParen TypeExpr TArrow TypeExpr TRParen
    { TRecordTypeErr $2 $4 }
  | TLBrace RecordFields TRBrace
    { TRecordErr $2 }
  | TVarRef
    { TVarErr $1 }

TVarRef :: { String }
  : TId
    { $1 }

RecordFields :: { [(String, TypeExpr)] }
  : RecordField
    { [$1] }
  | RecordField TComma RecordFields
    { $1 : $3 }
  |
    { [] }

RecordField :: { (String, TypeExpr) }
  : TId TEq TypeExpr
    { ($1, $3) }

TypeVars :: { [String] }
  :
    { [] }
  | TId TypeVars
    { $1 : $2 }

AnnotationDeclaration :: { Decl }
  : TId TDot TId TArrow TypeExpr
    { AnnotationDecl ($1 ++ "." ++ $3) $5 }
  | TId TArrow TypeExpr
    { AnnotationDecl $1 $3 }

ValueDeclaration :: { Decl }
  : TId Args TEq Expr
    { ValueDecl $1 (foldl EApp (EVar $1) ($2 ++ [$4])) }
  | TId TEq Expr
    { ValueDecl $1  $3  }  

Args :: { [Expr] }
  :
    { [] }
  | TId Args
    { EVar $1 : $2 }  -- Wrap TId into EVar and prepend to list


Expr :: { Expr }
  -- : Expr TOp Expr
  --   { case $2 of
  --       TOp s -> EOp $1 s $3 
  --       _     -> error "Unexpected operator token"
  --   }
  : TId
    { EVar $1 }
  | TLBrace Expr TRBrace
    { $2 }
  | TInt
    { EInt $1 }


IfExpr :: { Expr }
  : TIf Expr TThen Expr TElse Expr
    { EIf $2 $4 $6 }

LetExpr :: { Expr }
  : TLet LocalDefs TIn Expr
    { ELet $2 $4 }

LocalDefs :: { [LocalDef] }
  : LocalDef LocalDefs
    { $1 : $2 }
  |
    { [] }

LocalDef :: { LocalDef }
  : TId TEq Expr
    { LDef $1 $3 }

CaseExpr :: { Expr }
  : TCase Expr TOf CaseBranchList
    { ECase $2 $4 }

CaseBranchList :: { [(Pattern, Expr)] }
  : CaseBranch CaseBranchList
    { $1 : $2 }
  | CaseBranch
    { [$1] }

CaseBranch :: { (Pattern, Expr) }
  : Pattern TArrow Expr
    { ($1, $3) }

-- InfixExpr :: { Expr }
--   : AppExpr
--     { $1 }
--   | InfixExpr TOp AppExpr
--     { case $2 of
--         TOp s -> EOp $1 s $3
--         _     -> error "Unexpected operator token"
--     }

AppExpr :: { Expr }
  : Atom
    { $1 }
  | AppExpr Atom
    { EApp $1 $2 }

Atom :: { Expr }
  : TBool
    { EBool $1 }
  | TInt
    { EInt $1 }
  | TFloat
    { EFloat $1 }
  | TChar
    { EChar $1 }
  | TString
    { EString $1 }
  | TId
    { EVar $1 }
  | TLBracket ListContent TRBracket
    { EList $2 }
  | TLParen Expr TRParen
    { $2 }

ListContent :: { [Expr] }
  :
    { [] }
  | Expr MoreExpr
    { $1 : $2 }

MoreExpr :: { [Expr] }
  :
    { [] }
  | TComma Expr MoreExpr
    { $2 : $3 }

Pattern :: { Pattern }
  : TId
    { PVar $1 }
  | TUnderscore
    { PWildcard }
  | TInt
    { PLitInt $1 }
  | TBool
    { PLitBool $1 }
  | TLBracket PatternList TRBracket
    { PList $2 }

PatternList :: { [Pattern] }
  :
    { [] }
  | Pattern MorePattern
    { $1 : $2 }

MorePattern :: { [Pattern] }
  :
    { [] }
  | TComma Pattern MorePattern
    { $2 : $3 }
