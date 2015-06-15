{-# LANGUAGE OverloadedStrings #-}

module IR.Serialize where

import           Control.Applicative            ((<$>))
import           Control.Monad
import           Control.Monad.ST

import           Data.Aeson
import qualified Data.HashMap.Lazy              as M
import           Data.List
import           Data.Maybe
import           Data.STRef
import qualified Data.Text                      as T

import           IR

instance Show E where
  show (Const x) = x
  show (Ref x) = x
  show (Call f xs) = show f ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show (Lambda as s) = "\\(" ++ intercalate "," as ++ ") -> " ++ show s

instance Show S where
  show (Decl a x) = "var " ++ a ++ " = " ++ show x
  show (Assign a x) = a ++ " = " ++ show x
  show (Return x) = "return " ++ show x
  show (Ctrl f xs) = "ctrl(" ++ show f ++ ")" ++ show xs

instance Show P where
  show (P ss) = "{\n" ++ intercalate "\n" (map show ss) ++ "\n}"

instance Show L where
  show (Cnst c)   = "\"" ++ c ++ "\""
  show (Var n)    = n
  show (Extrn n)  = "‹" ++ n ++ "›"

  --show (App f x)  = "(" ++ show f ++ " " ++ show x ++ ")"
  --show (Lam n f)  = "(λ" ++ n ++ " → " ++ show f ++ ")"

  show (App f@(Lam _ _) x@(App _ _))  = "(" ++ show f ++ ") (" ++ show x ++ ")"
  show (App f x@(App _ _))            = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App f@(Lam _ _) x)            = "(" ++ show f ++ ") " ++ show x ++ ""
  show (App f x@(Lam _ _))            = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App f x)                      = "" ++ show f ++ " " ++ show x ++ ""
  show (Lam n f)                      = "λ" ++ n ++ " → " ++ show f ++ ""
