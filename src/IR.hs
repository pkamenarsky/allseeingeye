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
         | G_Arg Name
         | G_Assign Name (G a)
         | G_Return (G a)
         | G_Ctrl (G a) (G a)
         | G_Nop
         deriving (Eq, Ord, Show)

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

isPure :: G a -> Bool
isPure (G_Const c)    = True
isPure (G_ExtRef c)   = False
isPure (G_Call f xs)  = isPure f && all isPure xs
isPure (G_Lambda n f) = undefined
isPure (G_Decl n x)   = isPure x
isPure (G_Arg n)      = True
isPure (G_Assign n x) = isPure x
isPure (G_Return x)   = isPure x
isPure (G_Ctrl i x)   = isPure x
isPure (G_Nop)        = True
