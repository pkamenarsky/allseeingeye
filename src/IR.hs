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

data Label = L_Const String
           | L_ExtRef String
           | L_ClosureRef
           | L_Call
           | L_Lambda [Name]
           | L_Decl Name
           | L_Arg Name
           | L_Assign Name
           | L_Return
           | L_Ctrl
           | L_Nop
           deriving Show

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

type NodeId = Int

data G2 l a = G2 Int [(NodeId, l)] [(NodeId, NodeId)] deriving Show

emptyG2 :: G2 l a
emptyG2 = G2 0 [] []

addEdge :: NodeId -> NodeId -> G2 l a -> G2 l a
addEdge e1 e2 (G2 n ns es) = G2 n ns ((e1, e2):es)

addNode :: l -> G2 l a -> (NodeId, G2 l a)
addNode l (G2 n ns es) = (n, G2 (n + 1) ((n, l):ns) es)

addNodeM :: l -> State (G2 l a) NodeId
addNodeM l = do
  g <- get
  let (n, g') = addNode l g
  put g'
  return n

addEdgeM :: NodeId -> NodeId -> State (G2 l a) ()
addEdgeM e1 e2 = modify (addEdge e1 e2)

type Ctx2 a = String -> Maybe NodeId

ectx2 _ = Nothing

genG2fromE :: Ctx2 a -> E -> State (G2 Label a) NodeId
genG2fromE ctx (Const x) = addNodeM (L_Const x)
genG2fromE ctx (Ref r) | Just n <- ctx r = return n
                       | otherwise       = addNodeM (L_ExtRef r)
genG2fromE ctx (Call f xs) = do
  n   <- addNodeM L_Call
  fn  <- genG2fromE ctx f
  xns <- mapM (genG2fromE ctx) xs
  addEdgeM n fn
  mapM_ (addEdgeM n) xns
  return n
genG2fromE ctx (Lambda ns f) = do
  n      <- addNodeM (L_Lambda ns)
  nsm    <- zip ns <$> mapM (\n -> addNodeM (L_Arg n)) ns
  -- L_ClosureRef?
  -- (l, _) <- genG2fromS (\r -> lookup r nsm <|> ctx r) f
  (l, _) <- genG2fromS (\r -> lookup r nsm) f
  addEdgeM n l
  -- mapM_ (addEdgeM n . snd) nsm
  return n

genG2fromS :: Ctx2 a -> S -> (State (G2 Label a)) (NodeId, Ctx2 a)
genG2fromS ctx (Decl n x) = do
  nid <- addNodeM (L_Decl n)
  eid <- genG2fromE ctx x
  addEdgeM nid eid
  return (nid, \r -> if r == n then Just nid else ctx r)
genG2fromS ctx (Assign n x) = do
  nid <- addNodeM (L_Assign n)
  eid <- genG2fromE ctx x
  addEdgeM nid eid
  return (nid, \r -> if r == n then Just nid else ctx r)
genG2fromS ctx (Block ss) = go ctx ss
  where
    go ctx [] = (,ctx) <$> addNodeM L_Nop
    go ctx (x@(Return _):_) = genG2fromS ctx x
    go ctx (x:xs) = do
      (_, ctx') <- genG2fromS ctx x
      go ctx' xs
genG2fromS ctx (Return x) = do
  nid <- addNodeM L_Return
  eid <- genG2fromE ctx x
  addEdgeM nid eid
  return (nid, ctx)
genG2fromS ctx (Ctrl x s) = do
  nid        <- addNodeM L_Ctrl
  ge         <- genG2fromE ctx x
  (gs, ctx') <- genG2fromS ctx s
  addEdgeM nid ge
  addEdgeM nid gs
  return (nid, ctx')

removeSubgraph :: NodeId -> G2 l a -> G2 l a
removeSubgraph n (G2 next ns es) = G2 next (M.toList ns') [ (e1, e2) | (e2, es) <- M.toList es', e1 <- es ]
  where
    outmap   = M.fromListWith (++) [ (e1, [e2]) | (e1, e2) <- es ]

    (ns', es') = go ( M.fromList ns
                    , M.fromListWith (++) [ (e2, [e1]) | (e1, e2) <- es ]
                    ) n

    go (nmap, inmap) n' = foldl go (M.delete n' nmap, M.delete n' inmap)
                                   (fromMaybe [] $ M.lookup n' outmap)

-- inlineLambdasG2 :: G2 Label a -> G2 Label a
inlineLambdasG2 (G2 _ ns es) = matchCDL
  where
    nmap     = M.fromList ns

    outmap   = M.fromListWith (++) [ (e1, [e2]) | (e1, e2) <- es ]
    inmap    = M.fromListWith (++) [ (e2, [e1]) | (e1, e2) <- es ]

    matchCL  = [ e | e@(e1, e2) <- es
                   , Just (L_Call)     <- [M.lookup e1 nmap]
                   , Just (L_Lambda _) <- [M.lookup e2 nmap]
               ]

    matchCDL = [ (ei, (e1, e2, e3, e4))
               | (e1, e2) <- es
               , Just (L_Call)     <- [M.lookup e1 nmap]
               , Just (L_Decl _)   <- [M.lookup e2 nmap]

               , e3                <- fromMaybe [] $ M.lookup e2 outmap
               , Just (L_Lambda _) <- [M.lookup e3 nmap]

               , e4                <- fromMaybe [] $ M.lookup e3 outmap
               , Just L_Return     <- [M.lookup e4 nmap]

               , ei                <- fromMaybe [] $ M.lookup e1 inmap
               ]

removeOrphanDecls :: G2 Label a -> G2 Label a
removeOrphanDecls (G2 next ns es) = G2 next ns' es'
  where
    inmap    = M.fromListWith (++) [ (e2, [e1]) | (e1, e2) <- es ]
    outmap   = M.fromListWith (++) [ (e1, [e2]) | (e1, e2) <- es ]

    nmap     = M.fromList ns

    matchCL  = [ e | e@(e1, e2) <- es
                   , Just (L_Decl _) <- [M.lookup e1 nmap]
                   , Just (L_Decl _) <- [M.lookup e2 nmap]
                   , (length $ fromMaybe [] $ M.lookup e2 inmap) == 1
               ]
    rmvDecl (ns', es') (e1, e2)
             = (filter ((/= e2) . fst) ns', [ if e2 == re1 then (e1, re2) else (re1, re2) | (re1, re2) <- es' ])

    (ns', es') = foldl rmvDecl (ns, es) matchCL


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
