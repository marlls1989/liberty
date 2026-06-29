{
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Language.Liberty.Parser
Description : Happy parser for the Liberty @.lib@ format.

A Happy LALR parser that consumes a lazy 'ByteString' and produces a
@['Liberty']@ AST (see "Language.Liberty.AST"). The single public entry point
is 'parseLiberty'.

The parser is coupled directly to the Alex lexer in "Language.Liberty.Lex"
rather than to a precomputed token list: @%monad { Alex }@ runs the parse inside
the lexer monad, and @%lexer { lexwrap } { TEOF }@ pulls one token at a time via
'lexwrap', stopping on 'TEOF'. This means the input is lexed and parsed in a
single streaming pass.
-}
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

-- The list rules (ElementL, ArgL1) use LEFT recursion and prepend each new
-- item with `$2 : $1` / `$3 : $1`. Left recursion is the standard LALR idiom:
-- it parses in constant stack space, but the accumulated list comes out
-- reversed, so each wrapping rule (LibertyFile, ArgL, and the Group action via
-- `reverse $6`) applies `reverse` once to restore source order.

-- A whole file is a forest of top-level elements.
LibertyFile :: { [Liberty] }
LibertyFile : ElementL { reverse $1 }

-- A (possibly empty) sequence of elements, accumulated in reverse.
ElementL :: { [Liberty] }
ElementL : ElementL Element { $2 : $1 }
         | {- empty -}      { [] }

-- A single element maps to one of the three Liberty AST shapes:
--   group:           id ( args ) { children }
--   complex attrib:  id ( args ) ;
--   simple attrib:   id : value ;
Element :: { Liberty }
Element : id "(" ArgL ")" "{" ElementL "}" { Group $1 $3 $ reverse $6 }
        | id "(" ArgL ")" ";"              { EAtrib $1 $3 }
        | id ":" Val ";"                   { Atrib $1 $3 }

-- A (possibly empty) comma-separated argument list.
ArgL :: { [AttrVal] }
ArgL : ArgL1 { reverse $1 }
     |       { [] }

-- A non-empty comma-separated argument list, accumulated in reverse.
ArgL1 :: { [AttrVal] }
ArgL1 : ArgL1 "," Val { $3 : $1 }
      | Val           { [$1] }

-- A single value. NOTE: both `id` (bareword) and `str` (quoted) become StrVal,
-- so the AST does not record which syntax was used.
Val :: { AttrVal }
Val : id	{ StrVal $1 }
    | num	{ NumVal $1 }
    | str	{ StrVal $1 }

{
-- | The threaded-lexer hook named by @%lexer { lexwrap } { TEOF }@. Happy calls
-- it with a continuation; it scans the next token with 'alexMonadScan' and feeds
-- it on. Written point-free as @(alexMonadScan >>=)@, i.e.
-- @lexwrap k = alexMonadScan >>= k@.
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

-- | The parse-error handler named by @%error { happyError }@. Aborts the Alex
-- monad with a message naming the offending token.
happyError :: Token -> Alex a
happyError t = alexError $ "Parser error at token " ++ (show t)

-- | Parse Liberty source into a list of top-level 'Liberty' nodes.
--
-- Returns @Left err@ on a lex or parse error (via 'alexError' /
-- 'happyError'), or @Right forest@ on success. This is the library's main
-- entry point; @show@ on the result re-serialises it (see
-- "Language.Liberty.AST").
parseLiberty :: ByteString -> Either String [Liberty]
parseLiberty s = runAlex s parse
}
