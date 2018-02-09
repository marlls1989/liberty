module Language.Liberty.AST
  (
    Liberty(..),
    AttrVal(..)
  )where

data AttrVal = NumVal Float
             | StrVal String deriving (Eq, Ord)

data Liberty = Group  String [AttrVal] [Liberty]
             | Atrib  String AttrVal
             | EAtrib String [AttrVal]
             deriving (Eq, Ord)
