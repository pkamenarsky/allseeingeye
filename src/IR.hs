{-# LANGUAGE OverloadedStrings, TupleSections #-}

module IR where

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

type G_E = [G]

data G = G_Const String
       | G_ExtRef String
       | G_Call G [G]
       | G_Lambda [Name] G
       | G_Decl Name G
       | G_Assign Name G
       | G_Return G
       | G_Ctrl G G
       | G_Nop
       deriving Show

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

type Ctx = String -> G

ectx s = G_ExtRef s

genGfromE :: Ctx -> E -> G
genGfromE _ (Const x) = G_Const x
genGfromE ctx (Ref r) = ctx r
genGfromE ctx (Call f xs) = G_Call (genGfromE ctx f) (map (genGfromE ctx) xs)
genGfromE ctx (Lambda ns f) = G_Lambda ns (fst $ genG ctx f)

genG :: Ctx -> S -> (G, Ctx)
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

cmpG :: G -> G -> Bool
cmpG = undefined
