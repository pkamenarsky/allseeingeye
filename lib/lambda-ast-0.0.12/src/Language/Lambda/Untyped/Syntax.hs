{-# LANGUAGE DeriveDataTypeable #-}
{- | An Untyped Lambda Calculus AST -}
module Language.Lambda.Untyped.Syntax where
import Data.Data

-- | A polymorphic version of the AST to allow different symbol types
data GExpr a = Cnst String
             | Var a
             | App (GExpr a) (GExpr a)
             | Lam a         (GExpr a)
     deriving(Show, Eq, Data, Typeable)
             
-- | A common symbol type choice
type Sym  = String

-- | A common AST
type Expr = GExpr Sym




