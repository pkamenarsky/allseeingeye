{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, QuasiQuotes, TypeSynonymInstances #-}

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
       | W (M.Map Name L)
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

rewriteW :: L -> L
rewriteW (Cnst c)  = Cnst c
rewriteW (Var "ω") = W M.empty
rewriteW (Var c)   = Var c
rewriteW (Extrn c) = Extrn c
-- rewriteW (Var "↪ω" `App` Var k `App` v `App` W w) = W $ M.insert k v w
rewriteW (Var "↪ω" `App` Var k `App` w) = merge $ rewriteW w
  where merge (W w1 `App` W w2) = W $ M.unions [w1, w2]
        merge (v `App` W w)     = W $ M.insert k v w
        merge w                 = W $ M.singleton k w
rewriteW (f `App` x) = rewriteW f `App` rewriteW x
rewriteW (n `Lam` f) = n `Lam` rewriteW f
rewriteW (W w)       = W $ M.map rewriteW w

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
normalize e = e

mergeFn = "⤚"
worldFn = "↖ω"
worldUpFn = "↪ω"
result = "ρ"
object = "σ"
world = "ω"

universeL :: L -> [L]
universeL e@(Cnst _) = [e]
universeL e@(Var _) = [e]
universeL e@(App f x) = [e] ++ universeL x -- ++ universeL f
universeL e@(Lam _ f) = [e]

showL (Cnst c)   = c
showL (Var n)    = n
showL (Extrn n)  = "‹" ++ n ++ "›"

--showL (App f x)  = "(" ++ showL f ++ " " ++ showL x ++ ")"
--showL (Lam n f)  = "(λ" ++ n ++ " → " ++ showL f ++ ")"

showL (App f@(Lam _ _) x@(App _ _))  = "(" ++ showL f ++ ") (" ++ showL x ++ ")"
showL (App f x@(App _ _))            = "" ++ showL f ++ " (" ++ showL x ++ ")"
showL (App f@(Lam _ _) x)            = "(" ++ showL f ++ ") " ++ showL x ++ ""
showL (App f x@(Lam _ _))            = "" ++ showL f ++ " (" ++ showL x ++ ")"
showL (App f x)                      = "" ++ showL f ++ " " ++ showL x ++ ""
showL (Lam n f)                      = "λ" ++ n ++ " → " ++ showL f ++ ""

rewriteL :: L -> L
rewriteL (Cnst c)  = (Cnst c)
rewriteL (Var n)   = (Var n)
rewriteL (Extrn n) = (Extrn n)
-- ↖ω ρ (f … ω)      ≈ f … (WRONG!)
-- ↖ω ρ (f … (↪ω …)) ≈ f … (WRONG!)
#if 0
rewriteL e@(Var "↖ω" `App` Var "ρ" `App` r@(f `App` x))
  | Just r' <- go r = (\x -> trace ("BEFORE: " ++ showL e ++ "\nAFTER : " ++ showL r' ++ "\n") x) r'
  | otherwise       = Var "↖ω" `App` Var "ρ" `App` rewriteL r
  where go (f `App` (Var "ω"))          = Just f
        go (f `App` (Var "↪ω" `App` _)) = Just f
        go (f `App` x)                  = App f <$> go x
        go _                            = Nothing
-- ↖ω s (… (↪ω s v ω)) ≈ v
--
-- r = f(a) → (r, a, ω) = f(a, ω) → ω = f(a, ω)
--   return r → ↪ω ρ r (↪ω σ obj ω)
#endif
#if 0
rewriteL (Var "↖ω" `App` k `App` x)
  | Just x' <- go x = x'
  | otherwise       = Var worldFn `App` rewriteL k `App` rewriteL x
  where go (Var "↪ω" `App` uk `App` uv `App` ux)
           | k == uk   = Just uv
           | otherwise = go ux
        go (App f x) = go f <|> go x
        go _ = Nothing
#endif
{-
rewriteL (Var "get" `App` obj `App` field)
  | (value:_) <- [ value | (Var "set" `App` _ `App` field' `App` value) <- universeL obj, field == field' ] = rewriteL value
  | otherwise = (Var "get" `App` rewriteL obj `App` rewriteL field)
-}
#if 0
rewriteL (Var "get" `App` k `App` x)
  | Just x' <- go x = x'
  | otherwise       = Var "get" `App` rewriteL k `App` rewriteL x
  where go (Var "set" `App` uk `App` uv `App` ux)
           | k == uk   = Just uv
           | otherwise = go ux
        go _ = Nothing
#endif
rewriteL e@(Var "↖ω" `App` Var k `App` W w)
  | Just v <- M.lookup k w = v
  | otherwise              = e
rewriteL (App f x) = App (rewriteL f) (rewriteL x)
rewriteL (Lam n f) = Lam n (rewriteL f)
rewriteL (W w) = W $ M.map rewriteL w
-- ↖ω σ (push a e ω) ≈ push a e

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

simplify :: L -> L
simplify = fixpoint (rewriteL . rewriteW . normalize)

subtree :: L -> L -> Maybe L
#if 0
subtree (Var "↪ω" `App` Var "ρ" `App` x1) (Var "↪ω" `App` Var "ρ" `App` x2)
  = (Var "↪ω" `App` Var "ρ" `App`) <$> subtree x1 x2
subtree (Var "↪ω" `App` _) (Var "↪ω" `App` _) = Just $ Var world
#endif
subtree (Var "↪ω" `App` _) (Var "ω") = Just $ Var world
subtree (Var "ω") (Var "↪ω" `App` _) = Just $ Var world
subtree (Cnst c1) (Cnst c2)
  | c1 == c2  = Just $ Cnst c1
  | otherwise = Nothing
subtree (Var c1) (Var c2)
  | c1 == c2  = Just $ Var c1
  | otherwise = Nothing
subtree (App f1 xs1) (App f2 xs2) = App <$> subtree f1 f2 <*> subtree xs1 xs2
subtree (Lam n1 f1) (Lam n2 f2)
  | n1 == n2  = Lam n1 <$> subtree f1 f2
  | otherwise = Nothing
subtree _ _ = Nothing

tlength = length . universe

lmtree :: L -> L -> Maybe L
lmtree l@(App f1 xs1) r@(App f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree l f2
                            , lmtree l xs2
                            , lmtree r f1
                            , lmtree r xs1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l@(Lam n1 f1) r@(Lam n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree l f2
                            , lmtree r f1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l r = subtree l r

lmtree1 :: L -> L -> Maybe L
lmtree1 l@(App f1 xs1) r@(App f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree1 l f2
                            , lmtree1 l xs2
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree1 l@(Lam n1 f1) r@(Lam n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree1 l f2
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree1 l r = subtree l r
