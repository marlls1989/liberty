{-# LANGUAGE DeriveGeneric #-}
module Language.Liberty.AST
  (
    Liberty(..),
    AttrVal(..)
  )where

import GHC.Generics
import Data.List

data AttrVal = NumVal Float
             | StrVal  String deriving (Eq, Ord, Generic)

data Liberty = Group  String [AttrVal] [Liberty]
             | Atrib  String AttrVal
             | EAtrib String [AttrVal]
             deriving (Eq, Ord, Generic)

indent :: String -> String
indent a = intercalate "\n\t" ("" : lines a) ++ "\n"

scapeString :: String -> String
scapeString = concatMap scapeChar where
  scapeChar :: Char -> String
  scapeChar c
    | c == '"' = "\\\""
    | c == '\\' = "\\\\"
    | otherwise = [c]

instance Show AttrVal where
  show (NumVal a) = show a
  show (StrVal a) = "\"" ++ scapeString a ++ "\""

instance Show Liberty where
  show (Group kind attrs children) = kind ++ " (" ++ intercalate ", " (map show attrs) ++ ") {\n" ++
                                     indent (unlines $ map show children) ++ "}"
  show (Atrib name val) = name ++ " : " ++ show val ++ ";"
  show (EAtrib name vals) = name ++ " (" ++ intercalate ", " (map show vals) ++ ");"
