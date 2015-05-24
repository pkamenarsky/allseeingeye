{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, TypeSynonymInstances #-}

module IR where

import           Control.Applicative          ((<$>), (<*>), pure)

import           Data.Data
import qualified Data.Map                     as M
import           Data.Typeable

import           Data.Generics.Uniplate.Data

import           Graph

type Name = String

data CtrlType

data E = Const String
       | Ref String
       | Call E [E]
       | Lambda [Name] S

data S = Decl Name E
       | Assign Name E
       | Block [S]
       | Return E
       | Ctrl E S -- merge with block?

data G a = G_Const String
         | G_ExtRef String
         | G_Call (G a) [G a]
         | G_Lambda [Name] (G a)
         | G_Decl Name (G a)
         | G_Arg Name
         | G_Assign Name (G a)
         | G_Return (G a)
         | G_Ctrl (G a) (G a)
         | G_Nop
         deriving (Eq, Ord, Data, Typeable, Show)

instance Graph (G a) String where
  node (G_Const c)    = ("const " ++ c, [])
  node (G_ExtRef c)   = ("extref", [])
  node (G_Call f xs)  | G_ExtRef n <- f = (n, xs)
                      | otherwise       = ("call", f:xs)
  node (G_Lambda n f) = ("lambda", [f])
  node (G_Decl n x)   = ("var " ++ n, [x])
  node (G_Arg n)      = ("arg " ++ n, [])
  node (G_Assign n x) = (n ++ " =", [x])
  node (G_Return x)   = ("return", [x])
  node (G_Ctrl i x)   = ("ctrl", (i:[x]))
  node (G_Nop)        = ("nop", [])

type Ctx a = String -> G a

ectx s = G_ExtRef s

genGfromE :: Ctx a -> E -> G a
genGfromE _ (Const x) = G_Const x
genGfromE ctx (Ref r) = ctx r
genGfromE ctx (Call f xs) = G_Call (genGfromE ctx f) (map (genGfromE ctx) xs)
genGfromE ctx (Lambda ns f) = G_Lambda ns (fst $ genG (\r -> if any (== r) ns then G_Arg r else ctx r) f)

genG :: Ctx a -> S -> (G a, Ctx a)
genG ctx (Decl n x) = let g = G_Decl n (genGfromE ctx x) in (g, \r -> if r == n then g else ctx r)
genG ctx (Assign n x) = let g = G_Assign n (genGfromE ctx x) in (g, \r -> if r == n then g else ctx r)
genG ctx (Block ss) = go ctx ss
  where
    go ctx [] = (G_Nop, ctx)
    go ctx (x@(Return _):_) = genG ctx x
    go ctx (x:xs) = go (snd $ genG ctx x) xs
genG ctx (Return x) = (G_Return $ genGfromE ctx x, ctx)
genG ctx (Ctrl x s) = (G_Ctrl ge gs, ctx')
  where
    ge = genGfromE ctx x
    (gs, ctx') = genG ctx s

cmpG :: G a -> G a -> Bool
cmpG = undefined

inlineLambdas :: Data a => G a -> Maybe (G a)
inlineLambdas (G_Call l@(G_Lambda ns g) xs)
  | length ns /= length xs = Nothing
  | otherwise              = Just $ transform f g
  where
    nxmap = M.fromList $ zip ns xs
    f (G_Arg n') | Just x' <- M.lookup n' nxmap = G_Decl n' x'
                 | otherwise                    = G_Arg n'
inlineLambdas g@(G_Call f xs)  = G_Call <$> inlineLambdas f <*> sequence (map inlineLambdas xs)
inlineLambdas g@(G_Const c)    = Just $ g
inlineLambdas g@(G_ExtRef c)   = Just $ g
inlineLambdas g@(G_Lambda n f) = G_Lambda <$> pure n <*> inlineLambdas f
inlineLambdas g@(G_Decl n x)   = G_Decl <$> pure n <*> inlineLambdas x
inlineLambdas g@(G_Arg n)      = Just $ g
inlineLambdas g@(G_Assign n x) = G_Assign <$> pure n <*> inlineLambdas x
inlineLambdas g@(G_Return x)   = G_Return <$> inlineLambdas x
inlineLambdas g@(G_Ctrl i x)   = G_Ctrl <$> inlineLambdas i <*> inlineLambdas x
inlineLambdas g@(G_Nop)        = Just $ g











type RefCtx = [Name]

gatherDecls :: Data a => G a -> [Name] -> [Name]
gatherDecls g _ = [ n | (G_ExtRef n) <- universe g ]

isPure :: RefCtx -> G a -> Bool
isPure ctx (G_Const c)    = True
isPure ctx (G_ExtRef c)   = False
isPure ctx (G_Call f xs)  = undefined
isPure ctx (G_Lambda n f) = isPure ctx f
isPure ctx (G_Decl n x)   = isPure (n:ctx) x
isPure ctx (G_Arg n)      = True
isPure ctx (G_Assign n x) = n `elem` ctx && isPure ctx x
isPure ctx (G_Return x)   = isPure ctx x
isPure ctx (G_Ctrl i x)   = isPure ctx x
isPure ctx (G_Nop)        = True
