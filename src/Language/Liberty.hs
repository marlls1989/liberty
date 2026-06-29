{-|
Module      : Language.Liberty
Description : Public entry point for the Liberty @.lib@ parser, AST and serialiser.

Umbrella module: it simply re-exports "Language.Liberty.Parser" (the parser,
@parseLiberty@) and "Language.Liberty.AST" (the 'Liberty' \/ 'AttrVal' types and
their serialising 'Show' instances). Import this single module to get the whole
public surface. This is the name @hsncl@'s @genLiberateTemplate@ imports as
@Language.Liberty@.
-}
module Language.Liberty (
  module Language.Liberty.Parser,
  module Language.Liberty.AST
  ) where

import Language.Liberty.Parser
import Language.Liberty.AST
