{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, QuasiQuotes, TypeSynonymInstances #-}

module IR where

import           Control.Applicative          ((<$>), (<*>), (<|>), pure)
import           Control.Monad
import           Control.Monad.State

import           Data.Data
import           Data.Function
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Typeable

import           Data.Generics.Uniplate.Data

import           Graph

{-
import           Language.Lambda.Untyped.Parser
import           Language.Lambda.Untyped.Syntax
import           Language.Lambda.Untyped.Quote
-}

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
       | Extrn Name
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
  where go (Lam n e) x' = normalize $ subst n x' e
        go f' x'        = App f' x'
normalize (Lam n f) = go (normalize f)
  where
        {-
        go f'@(Var n') | n == n'   = Var n
                       | otherwise = Lam n f'
        -}
        go f' = Lam n f'

rewriteL :: L -> L
rewriteL (Cnst c)  = (Cnst c)
rewriteL (Var n)   = (Var n)
rewriteL (App (App (Var "get") obj) field)
-- rewriteL [lam| get *obj *field |]
  | [value] <- [ value | (App (App (App (Var "set") _) field') value) <- universe obj, field == field' ] = value
  | otherwise = (App (App (Var "get") (rewriteL obj)) (rewriteL field))
rewriteL (App (App (Var "merge") (App (Var "get_result") x)) y)
  |  null [ () | Var "world" <- universe x' ]
  && null [ () | Var "world" <- universe y' ] = x'
  | otherwise = (App (App (Var "merge") (App (Var "get_result") x')) y')
    where x' = rewriteL x
          y' = rewriteL y
rewriteL (App (App (Var "merge") x) y)
  |  null [ () | Var "world" <- universe x' ]
  && null [ () | Var "world" <- universe y' ] = x'
  | otherwise = (App (App (Var "merge") x') y')
    where x' = rewriteL x
          y' = rewriteL y
rewriteL (App f x) = App (rewriteL f) (rewriteL x)
rewriteL (Lam n f) = Lam n (rewriteL f)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

simplify :: L -> L
simplify = fixpoint (rewriteL . normalize)

subtree :: L -> L -> Maybe L
subtree (Cnst c1) (Cnst c2)
  | c1 == c2  = Just $ Cnst c1
  | otherwise = Nothing
subtree (Var c1) (Var c2)
  | c1 == c2  = Just $ Var c1
  | otherwise = Nothing
subtree (App f1 xs1) (App f2 xs2)
  | Just f  <- subtree f1 f2
  , Just xs <- subtree xs1 xs2 = Just $ App f xs
  | otherwise = Nothing
subtree (Lam n1 f1) (Lam n2 f2)
  | n1 == n2
  , Just f <- subtree f1 f2 = Just $ Lam n1 f
  | otherwise = Nothing
subtree _ _ = Nothing

tlength = length . universe

lmtree :: L -> L -> Maybe L
lmtree l@(App f1 xs1) r@(App f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , subtree l f2
                            , subtree l xs2
                            , subtree r f1
                            , subtree r xs1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l@(Lam n1 f1) r@(Lam n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , subtree l f2
                            , subtree r f1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l r = subtree l r
