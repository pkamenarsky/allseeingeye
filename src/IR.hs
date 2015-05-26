{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, TypeSynonymInstances #-}

module IR where

import           Control.Applicative          ((<$>), (<*>), pure)
import           Control.Monad
import           Control.Monad.State

import           Data.Data
import qualified Data.Map                     as M
import           Data.Maybe
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

type Label = String

type NodeId = Int

data G2 l a = G2 [(NodeId, l)] [(NodeId, NodeId)]

emptyG2 :: G2 l a
emptyG2 = undefined

addEdge :: NodeId -> NodeId -> G2 l a -> G2 l a
addEdge = undefined

addNode :: l -> G2 l a -> (NodeId, G2 l a)
addNode = undefined

addNodeM :: l -> State (G2 l a) NodeId
addNodeM = undefined

addEdgeM :: NodeId -> NodeId -> State (G2 l a) ()
addEdgeM = undefined

getVert :: l -> G2 l a -> [NodeId]
getVert = undefined

getDecl :: String -> G2 l a -> Maybe NodeId
getDecl = undefined

type Ctx2 a = String -> NodeId

genG2fromE :: Ctx2 a -> E -> State (G2 Label a) NodeId
genG2fromE ctx (Const x) = addNodeM "const"
genG2fromE ctx (Ref r) = return $ ctx r
genG2fromE ctx (Call f xs) = do
  n  <- addNodeM "call"
  fg <- genG2fromE ctx f
  ns <- foldM (\ns x -> (:ns) <$> genG2fromE ctx x) [] xs
  mapM_ (addEdgeM n) ns
  return n
genG2fromE ctx (Lambda ns f) = do
  n   <- addNodeM "lambda"
  nsm <- zip ns <$> mapM (\n -> addNodeM "arg") ns
  l   <- genG2fromS (\r -> fromMaybe (ctx r) $ lookup r nsm) f
  addEdgeM n l
  return n

genG2fromS :: Ctx2 a -> S -> State (G2 Label a) NodeId
genG2fromS = undefined

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
-- TODO: G_Assign & rename names & aliasing
inlineLambdas (G_Call (G_Decl _ l@(G_Lambda ns (G_Return g))) xs)
  | length ns /= length xs = Nothing
  | otherwise              = Just $ transform f g
  where
    nxmap = M.fromList $ zip ns xs
    f (G_Arg n') | Just x' <- M.lookup n' nxmap = G_Decl n' x'
                 | otherwise                    = G_Arg n'
    f x = x
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

incomingEdges :: G a -> M.Map (G a) Int
incomingEdges = undefined

simplify :: Data a => G a -> G a
simplify = undefined

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
