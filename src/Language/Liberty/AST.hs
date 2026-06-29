{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Language.Liberty.AST
Description : Abstract syntax tree for the Liberty @.lib@ format, and the serialiser that emits it.

This module defines the in-memory representation of a parsed Liberty
standard-cell file ('Liberty' / 'AttrVal') together with the code that turns
that representation back into Liberty text.

The serialiser is /not/ a separate function: the 'Show' instances for 'Liberty'
and 'AttrVal' below ARE the @.lib@ writer. @show@ on a @['Liberty']@ (or on a
single node) produces valid Liberty syntax, so the round trip is
@parseLiberty@ → 'Liberty' AST → @show@ → @.lib@ text.

Both 'Liberty' and 'AttrVal' derive 'Generic'. This is intentional and must be
kept: downstream code in @hsncl@ (notably @genLiberateTemplate@) uses generic
deriving to serialise/manipulate these values, so the instances are not dead.
-}
module Language.Liberty.AST
  (
    Liberty(..),
    AttrVal(..)
  )where

import GHC.Generics
import Data.List

-- | A Liberty attribute value: either a numeric literal or a string\/identifier.
--
-- NOTE: the parser maps both quoted Liberty strings and barewords
-- (identifiers) to 'StrVal', so the AST cannot tell a quoted @"foo"@ apart from
-- a bareword @foo@. Quoted strings additionally retain their surrounding double
-- quotes /inside/ the 'String' (see 'Language.Liberty.Lex'), which is what makes
-- a quoted value round-trip; barewords carry no quotes.
data AttrVal = NumVal Float -- ^ A number. NOTE: stored as single-precision
                            -- 'Float', so very precise inputs may lose
                            -- precision and not byte-match the original text on
                            -- re-emit.
             | StrVal  String -- ^ A string or identifier; quoted strings keep
                              -- their surrounding quote characters in the payload.
                              deriving (Eq, Ord, Generic)

-- | A node of a Liberty file. A Liberty file is a forest of these (@['Liberty']@).
--
-- The three shapes correspond directly to the three grammar productions in
-- "Language.Liberty.Parser" and to the three Liberty surface syntaxes
-- serialised by the 'Show' instance below.
data Liberty = Group  String [AttrVal] [Liberty]
               -- ^ A group: @kind (attrs) { children }@. The first field is the
               -- group kind (e.g. @library@, @cell@, @pin@), the second its
               -- parenthesised arguments, the third its nested children.
             | Atrib  String AttrVal
               -- ^ A simple attribute: @name : value;@.
             | EAtrib String [AttrVal]
               -- ^ A complex\/parenthesised attribute: @name (v, …);@.
             deriving (Eq, Ord, Generic)

-- | Prefix every line of the input with a single tab. Used by the 'Group'
-- serialiser to indent its children; because each nesting level applies it
-- once more, a node at depth @N@ ends up with @N@ leading tabs.
indent :: String -> String
indent = unlines . map ('\t':) . lines

-- | Escape @"@ and @\\@ in a string for safe quoting inside Liberty syntax.
--
-- NOTE: this is DEAD CODE — it is defined but never called anywhere. Because
-- the @'Show' 'AttrVal'@ instance emits @StrVal@ payloads verbatim
-- (@show (StrVal a) = a@), the serialiser is /not/ escape-safe: any embedded
-- quote or backslash that is not already present as raw bytes in the AST will
-- be emitted unescaped. (The name "scape" is a misspelling of "escape"; it is
-- unexported, so flagged here but deliberately not renamed.)
scapeString :: String -> String
scapeString = concatMap scapeChar where
  scapeChar :: Char -> String
  scapeChar c
    | c == '"' = "\\\""
    | c == '\\' = "\\\\"
    | otherwise = [c]

-- | Serialise an 'AttrVal' to Liberty text.
--
-- NOTE: @StrVal@ is emitted verbatim, with no escaping (see 'scapeString',
-- which is dead code). Round-tripping a quoted string therefore relies on the
-- surrounding quote bytes already being part of the stored 'String'.
instance Show AttrVal where
  show (NumVal a) = show a
  show (StrVal a) = a

-- | Serialise a 'Liberty' node to Liberty text. This instance IS the @.lib@
-- writer; @show@ on a node yields valid Liberty syntax. The contract:
--
--   * 'Atrib'  @name val@      → @name : val;@
--   * 'EAtrib' @name vals@     → @name (v, …);@  (values comma-separated)
--   * 'Group'  @kind attrs cs@ → @kind (attrs) {\\n …indented children… }@
--
-- Children are rendered one per line and run through 'indent', so nesting
-- compounds to @N@ tabs at depth @N@.
instance Show Liberty where
  show (Group kind attrs children) = kind ++ " (" ++ intercalate ", " (map show attrs) ++ ") {\n" ++
                                     indent (unlines $ map show children) ++ "}"
  show (Atrib name val) = name ++ " : " ++ show val ++ ";"
  show (EAtrib name vals) = name ++ " (" ++ intercalate ", " (map show vals) ++ ");"
