{
{-# LANGUAGE OverloadedStrings #-}
module Language.Liberty.Lex
  (Alex(..),
   runAlex,
   alexError,
   alexMonadScan,
   Token(..)) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monad-bytestring"

@white = [ \t\n\r]+
@id = [A-Za-z\!\@\#\$\%\&_\+\|~\?\^] [A-Za-z0-9\!\@\#\$\%\^\&_\+\|~\?:]*
@comment = ("//" [^\n\r]*) | ("/*" ([^\*]|('*'[^\/]))* "*/") | (\\[ \t]*((\r\n)|[\r\n]))
@number = [\-\+]?([0-9]+\.?[0-9]*([Ee][\-\+]?[0-9]+)?|[0-9]*\.?[0-9]+([Ee][\-\+]?[0-9]+)?)
@str = '"' (. # '"' | '\\"' )* '"'

tokens :-
  @white	;
  @comment	;
  "("		{ retToken (\_ -> TParL) }
  ")"		{ retToken (\_ -> TParR) }
  "{"		{ retToken (\_ -> TBraceL) }
  "}"		{ retToken (\_ -> TBraceR) }
  ";"		{ retToken (\_ -> TSemiCol) }
  ":"		{ retToken (\_ -> TCol) }
  ","		{ retToken (\_ -> TComma) }
  @id		{ retToken (\s -> TId s) }
  @number	{ retToken (\s -> TNum $ read s) }
  @str		{ retToken (\s -> TStr s) }

{
data Token = TCol
           | TSemiCol
           | TComma
           | TParL
           | TParR
           | TBraceL
           | TBraceR
           | TEOF
           | TNum Float
           | TId String
           | TStr String deriving (Eq, Ord, Show)

alexEOF :: Alex Token
alexEOF = return TEOF

retToken :: (String -> Token) -> AlexAction Token
retToken f = (\(_,_,s,_) i -> return $ f (B.unpack $ B.take i s))
}
