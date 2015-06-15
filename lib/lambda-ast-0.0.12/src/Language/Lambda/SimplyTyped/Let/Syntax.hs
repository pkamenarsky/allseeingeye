{-# LANGUAGE DeriveDataTypeable #-}
module Language.Lambda.SimplyTyped.Let.Syntax where
import Data.Data

data Type a = Base a | Arrow (Type a) (Type a)
     deriving(Show, Eq, Data, Typeable)

data Expr s a c = Var s
                | App (Expr s a c) (Expr s a c)
                | Lam s (Type a) (Expr s a c)
                | Constant c
                | Let s (Expr s a c) (Expr s a c)
     deriving(Show, Eq, Data, Typeable)