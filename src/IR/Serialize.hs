{-# LANGUAGE CPP, OverloadedStrings #-}

module IR.Serialize where

import           Control.Applicative            ((<$>))
import           Control.Monad
import           Control.Monad.ST

import           Data.Aeson
import qualified Data.HashMap.Lazy              as M
import qualified Data.Map                       as Map
import           Data.List
import           Data.Maybe
import           Data.STRef
import qualified Data.Text                      as T

import           IR

instance Show Name where
  show (Global n) = "G" ++ n
  show (Local n) =  "L" ++ n
  show (Bound n) =  "B" ++ n

instance Show E where
  show (Const x) = x
  show (Ref x) = show x
  show (Call f xs) = show f ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show (Lambda as s) = "\\(" ++ intercalate "," (map show as) ++ ") -> " ++ show s

instance Show S where
  show (Decl a x) = "var " ++ show a ++ " = " ++ show x
  show (Assign a x) = show a ++ " = " ++ show x
  show (Return x) = "return " ++ show x
  show (Ctrl f xs) = "ctrl(" ++ show f ++ ")" ++ show xs

instance Show P where
  show (P ss) = "{\n" ++ intercalate "\n" (map show ss) ++ "\n}"

instance Show W where
#if 0
  show (W w) = "⟦" ++ intercalate ", " (map (\(k, v) -> k ++ " → " ++ show v) $ Map.toList w) ++ "⟧"
#else
  show (W w) = "(W " ++ intercalate " " (map (\(k, v) -> show k ++ " → " ++ show v) $ Map.toList w) ++ ")"
#endif

instance Show a => Show (L a) where
  show (Cnst a c)   = c
  show (Var a n)    = show n
  show (Extrn a n)  = "⟨" ++ show n ++ "⟩"

#if 0
  show (App a f x)  = "(" ++ show f ++ " " ++ show x ++ show a ++ ")"
  show (Lam a n f)  = "(λ" ++ n ++ " → " ++ show f ++ show a ++ ")"
#else
  show (App a f@(Lam _ _ _) x@(App _ _ _))  = "(" ++ show f ++ ") (" ++ show x ++ ")"
  show (App a f x@(App _ _ _))              = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App a f@(Lam _ _ _) x)              = "(" ++ show f ++ ") " ++ show x ++ ""
  show (App a f x@(Lam _ _ _))              = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App a f x)                          = "" ++ show f ++ " " ++ show x ++ ""
  show (Lam a n f)                          = "λ" ++ show n ++ " → " ++ show f ++ ""
  show (Merge a xs)                         = "(⤚ " ++ intercalate " " (map (\(k, v) -> k ++ "⟨" ++ show v ++ "⟩") $ Map.toList xs) ++ ")"
#endif
