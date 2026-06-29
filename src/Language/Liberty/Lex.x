{
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Language.Liberty.Lex
Description : Alex lexer for the Liberty @.lib@ format.

An Alex lexer (@%wrapper "monad-bytestring"@) that turns a lazy 'ByteString' of
Liberty source into a stream of 'Token's. It runs inside the Alex monad and is
driven token-by-token by "Language.Liberty.Parser" via @alexMonadScan@; no token
list is ever materialised.

Re-exports from the Alex monad wrapper ('Alex', 'runAlex', 'alexError',
'alexMonadScan') are bundled here alongside the 'Token' type so the parser can
import everything it needs from one place.
-}
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

-- @white: runs of whitespace; discarded (see the rules below).
@white = [ \t\n\r]+
-- @id: an identifier. NOTE: the allowed symbol set is unusually broad — the
-- first char may be a letter or any of ! @ # $ % & _ + | ~ ? ^, and subsequent
-- chars additionally allow digits and ':'. This is wider than the Liberty spec
-- strictly requires and lets many bareword tokens through.
@id = [A-Za-z\!\@\#\$\%\&_\+\|~\?\^] [A-Za-z0-9\!\@\#\$\%\^\&_\+\|~\?:]*
-- @comment: three alternatives — (1) "//" line comment, (2) "/* ... */" block
-- comment, and (3) a backslash line-continuation (a '\' followed by optional
-- spaces/tabs then a newline). All are discarded.
@comment = ("//" [^\n\r]*) | ("/*" ([^\*]|('*'[^\/]))* "*/") | (\\[ \t]*((\r\n)|[\r\n]))
-- @number: an optionally-signed integer or float, with an optional exponent.
@number = [\-\+]?([0-9]+\.?[0-9]*([Ee][\-\+]?[0-9]+)?|[0-9]*\.?[0-9]+([Ee][\-\+]?[0-9]+)?)
-- @str: a double-quoted string allowing escaped \" inside. NOTE: the matched
-- token KEEPS its surrounding double quotes (they are part of the token bytes),
-- which is what later lets a quoted value round-trip through the AST.
@str = '"' (. # '"' | '\\"' )* '"'

tokens :-
  -- whitespace and comments are matched then thrown away (empty action)
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
-- | A lexical token of the Liberty grammar.
data Token = TCol      -- ^ @:@
           | TSemiCol  -- ^ @;@
           | TComma    -- ^ @,@
           | TParL     -- ^ @(@
           | TParR     -- ^ @)@
           | TBraceL   -- ^ @{@
           | TBraceR   -- ^ @}@
           | TEOF      -- ^ end of input (returned by 'alexEOF'; the parser's
                       -- @%lexer@ uses this as its stop token)
           | TNum Float  -- ^ a numeric literal (single-precision 'Float')
           | TId String  -- ^ a bareword identifier
           | TStr String -- ^ a quoted string, /including/ its surrounding quotes
                         deriving (Eq, Ord, Show)

-- | The end-of-input action required by the monad wrapper; yields 'TEOF'.
alexEOF :: Alex Token
alexEOF = return TEOF

-- | Build an 'AlexAction' that constructs a 'Token' from the matched text.
--
-- The 'AlexInput' is the wrapper's 4-tuple @(pos, prevChar, remainingInput, bytePos)@;
-- only the remaining-input bytestring @s@ is needed, so the rest are matched
-- with @_@. @i@ is the length (in bytes) of the current match, so
-- @B.take i s@ slices off exactly the matched lexeme, which is unpacked to a
-- 'String' and handed to @f@.
retToken :: (String -> Token) -> AlexAction Token
retToken f = (\(_,_,s,_) i -> return $ f (B.unpack $ B.take i s))
}
