{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, TypeSynonymInstances #-}

module IR where

import           Control.Applicative          ((<$>), (<*>), (<|>), pure)
import           Control.Monad
import           Control.Monad.State

import           Data.Data
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Typeable

import           Data.Generics.Uniplate.Data

import           Graph

import Debug.Trace

type Name = String

data CtrlType

data E = Const String
       | Ref String
       | Call E [E]
       | Lambda [Name] P
       deriving (Eq, Ord, Data, Typeable)

data S = Decl Name E
       | Assign Name E
       | Return E
       | Ctrl E P
       deriving (Eq, Ord, Data, Typeable)

data P = P [S] deriving (Eq, Ord, Data, Typeable)

data L = Cnst String
       | Var Name
       | App L L
       | Lam Name L
       deriving (Eq, Ord, Data, Typeable)

sToE :: E -> L
sToE (Const c)     = Cnst c
sToE (Ref r)       = Var r
sToE (Call f xs)   = foldl App (sToE f) (map sToE xs)
sToE (Lambda ns p) = foldr Lam (sToP p) ns

sToP :: P -> L
sToP (P [])              = error "no return"
sToP (P (Decl n e:ps))   = App (Lam n (sToP (P ps))) (sToE e)
sToP (P (Assign n e:ps)) = App (Lam n (sToP (P ps))) (sToE e)
sToP (P (Return e:_))    = sToE e
sToP (P (Ctrl e p:_))    = error "ctrl"

subst :: Name -> L -> L -> L
subst _ _ e'@(Cnst _) = e'
subst n e e'@(Var n') | n == n'   = e
                      | otherwise = e'
subst n e e'@(App f x)  = App (subst n e f) (subst n e x)
subst n e e'@(Lam n' f) = Lam n' (subst n e f)

normalize :: L -> L
normalize (Cnst c)  = (Cnst c)
normalize (Var n)   = (Var n)
normalize (App f x) = go (normalize f) (normalize x)
  where go (Lam n e) x' = subst n x' e
        go f' x'        = App f' x'
normalize (Lam n f) = go (normalize f)
  where
        {-
        go f'@(Var n') | n == n'   = Var n
                       | otherwise = Lam n f'
        -}
        go f' = Lam n f'
