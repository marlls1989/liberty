{
{-# LANGUAGE OverloadedStrings #-}
module Language.Liberty.Parser (parseLiberty) where

import Data.Either
import Data.List
import Language.Liberty.Lex
import Language.Liberty.AST
import Data.ByteString.Lazy (ByteString)
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { TEOF }
%error { happyError }

%token
  id	{ TId $$ }
  num	{ TNum $$ }
  str	{ TStr $$ }
  "("	{ TParL }
  ")"	{ TParR }
  "{"	{ TBraceL }
  "}"	{ TBraceR }
  ","	{ TComma }
  ":"	{ TCol }
  ";"	{ TSemiCol }

%%

LibertyFile :: { [Liberty] }
LibertyFile : ElementL { reverse $1 }

ElementL :: { [Liberty] }
ElementL : ElementL Element { $2 : $1 }
         | {- empty -}      { [] }

Element :: { Liberty }
Element : id "(" ArgL ")" "{" ElementL "}" { Group $1 $3 $ reverse $6 }
        | id "(" ArgL ")" ";"              { EAtrib $1 $3 }
        | id ":" Val ";"                   { Atrib $1 $3 }

ArgL :: { [AttrVal] }
ArgL : ArgL1 { reverse $1 }
     |       { [] }

ArgL1 :: { [AttrVal] }
ArgL1 : ArgL1 "," Val { $3 : $1 }
      | Val           { [$1] }

Val :: { AttrVal }
Val : id	{ StrVal $1 }
    | num	{ NumVal $1 }
    | str	{ StrVal $1 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

happyError :: Token -> Alex a
happyError t = alexError $ "Parser error at token " ++ (show t)

parseLiberty :: ByteString -> Either String [Liberty]
parseLiberty s = runAlex s parse
}
