{
module Lexer where

import Types (Token(..))
import Data.Char (isAlpha, isAlphaNum, isDigit)
}

%wrapper "monad"

$digit      = 0-9
$alpha      = [a-zA-Z]
$alphanum   = [$alpha $digit \_]
$symbol     = [\+ \- \* \/ \= \> \< \! \? \: \. \, \; \& \^ \% \$ \# \@ \~ \[ \] \{ \} \( \) \- \_]

tokens :-

  "module"                          { \_ _ -> TModule }
  "import"                          { \_ _ -> TImport }
  "exposing"                        { \_ _ -> TExposing }
  "type"                            { \_ _ -> TType }
  "alias"                           { \_ _ -> TAlias }
  "case"                            { \_ _ -> TCase }
  "of"                              { \_ _ -> TOf }
  "if"                              { \_ _ -> TIf }
  "then"                            { \_ _ -> TThen }
  "else"                            { \_ _ -> TElse }
  "let"                             { \_ _ -> TLet }
  "in"                              { \_ _ -> TIn }
  "{"                               { \_ _ -> TLBrace }
  "}"                               { \_ _ -> TRBrace }
  "["                               { \_ _ -> TLBracket }
  "]"                               { \_ _ -> TRBracket }
  "("                               { \_ _ -> TLParen }
  ")"                               { \_ _ -> TRParen }
  ","                               { \_ _ -> TComma }
  "|"                               { \_ _ -> TPipe }
  "."                               { \_ _ -> TDot }
  "="                               { \_ _ -> TEq }
  "->"                              { \_ _ -> TArrow }
  "_"                               { \_ _ -> TUnderscore }
  "True"                            { \_ _ -> TBool True }
  "False"                           { \_ _ -> TBool False }
  [$digit]+                         { \input len -> TInt (read input) }
  [$digit]+"."[$digit]+             { \input len -> TFloat (read input) }
  "'"[^'\n']"'"                     { \input len -> TChar (read input) }
  "\""[^"\n"]*"\""                  { \input len -> TString (read input) }
  [$symbol]+                        { \s _ -> TOp s }
  [$alpha][$alphanum]*              { \s _ -> TId s }
  "--".*                            ;
  $white+                           ;
  .
{

alexEOF :: Alex Token
alexEOF = error "End of file reached"

-- Scanner function
alexScanTokens :: String -> [Token]
alexScanTokens str = case runAlex str alexMonadScan of
  Left err   -> error err
  Right toks -> toks
}
