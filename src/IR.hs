{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, TypeSynonymInstances #-}

module IR where

import Graph

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
       | G_Assign Name (G a)
       | G_Return (G a)
       | G_Ctrl (G a) (G a)
       | G_Nop
       deriving (Eq, Ord, Show)

instance Graph (G a) String where
  node (G_Const c)    = ("const", [])
  node (G_ExtRef c)   = ("extref", [])
  node (G_Call f xs)  = ("call", (f:xs))
  node (G_Lambda n f) = ("lambda", [f])
  node (G_Decl n x)   = ("decl", [x])
  node (G_Assign n x) = ("assgn", [x])
  node (G_Return x)   = ("return", [x])
  node (G_Ctrl i x)   = ("ctrl", (i:[x]))
  node (G_Nop)        = ("nop", [])

{-
cmpE :: E -> E -> Bool
cmpE (Const x) (Const y) = x == y
cmpE (Ref x) (Ref y) = x == y
cmpE (Call f xs) (Call g ys) = (cmpE f g) -- && intersect
cmpE (Lambda as s) (Lambda bs t) = (cmpS s t) -- && intersect
cmpE _ _ = False

cmpS :: S -> S -> Bool
cmpS (Decl a x) (Decl b y) = a == b && cmpE x y
cmpS (Assign a x) (Assign b y) = a == b && cmpE x y
cmpS x@(Block _) y@(Block _) = genG x `cmpG` genG y
cmpS (Return x) (Return y) = cmpE x y
cmpS (Ctrl x u) (Ctrl y v) = cmpE x y && cmpS u v
-}

type Ctx a = String -> G a

ectx s = G_ExtRef s

genGfromE :: Ctx a -> E -> G a
genGfromE _ (Const x) = G_Const x
genGfromE ctx (Ref r) = ctx r
genGfromE ctx (Call f xs) = G_Call (genGfromE ctx f) (map (genGfromE ctx) xs)
genGfromE ctx (Lambda ns f) = G_Lambda ns (fst $ genG ctx f)

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
