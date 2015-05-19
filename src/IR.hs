{-# LANGUAGE OverloadedStrings, TupleSections #-}

module IR where

import           Control.Applicative  ((<$>))
import           Control.Monad.ST

import           Data.Aeson
import           Data.List
import qualified Data.HashMap.Lazy     as M
import           Data.STRef
import qualified Data.Text             as T

import           System.Random
import           System.IO.Unsafe

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

type DeSer = G -> Int

serializeG :: G -> Value
serializeG g = runST $ do
  nid   <- newSTRef (0 :: Int)
  eid   <- newSTRef (0 :: Int)
  nmap  <- newSTRef M.empty
  verts <- newSTRef M.empty
  edges <- newSTRef []

  let insNode x obj = do
        xid <- case x of
          Just x' -> M.lookup x' <$> readSTRef nmap
          Nothing -> return Nothing

        case xid of
          Just xid' -> return xid'
          Nothing   -> do
            xid' <- readSTRef nid

            modifySTRef nid (+1)
            modifySTRef verts (M.insert xid' $ object $ [ "id" .= ("n" ++ show xid'), "size" .= (3 :: Int), "x" .= (unsafePerformIO $ randomRIO (1, 100) :: Int), "y" .= (unsafePerformIO $ randomRIO (1, 100) :: Int) ] ++ obj)

            case x of
              Just x' -> modifySTRef nmap (M.insert x' xid')
              Nothing -> return ()

            return xid'

      insEdge fromId toG = do
        toId <- ser toG
        eid' <- readSTRef eid
        modifySTRef eid (+1)
        modifySTRef edges ((eid', fromId, toId):)
        return toId

      ser (G_Const c) = insNode Nothing [ "label" .= ("const " ++ c) ]
      ser (G_ExtRef x) = insNode (Just x) [ "label" .= ("extrn " ++ x) ]
      ser (G_Call f xs) = do
        xid <- insNode Nothing [ "label" .= ("call" :: String) ]
        fid <- insEdge xid f
        mapM_ (insEdge fid) xs
        return xid
      ser (G_Lambda ns f) = do
        xid <- insNode Nothing [ "label" .= ("\\" ++ (intercalate "," ns) ++ " ->") ]
        insEdge xid f
        return xid
      ser (G_Decl x s) = do
        xid <- insNode (Just x) [ "label" .= ("var " ++ x) ]
        insEdge xid s
        return xid
      ser (G_Assign x s) = do
        xid <- insNode (Just x) [ "label" .= (x ++ "=") ]
        insEdge xid s
        return xid
      ser (G_Return x) = do
        xid <- insNode Nothing [ "label" .= ("return" :: String) ]
        insEdge xid x
        return xid
      ser (G_Ctrl x y) = do
        xid <- insNode Nothing [ "label" .= ("ctrl" :: String) ]
        insEdge xid x
        insEdge xid y
        return xid
      ser (G_Nop) = insNode Nothing [ "label" .= ("nop" :: String) ]

  ser g

  verts' <- readSTRef verts
  edges' <- readSTRef edges

  return $ object [ "nodes" .= M.elems verts'
                  , "edges" .= [ object [ "id"     .= ("e" ++ show eid)
                                        , "source" .= ("n" ++ show source)
                                        , "target" .= ("n" ++ show target)
                                        ]
                               | (eid, source, target) <- edges'
                               ]
                  ]

cmpG :: G -> G -> Bool
cmpG = undefined

---

instance Show E where
  show (Const x) = x
  show (Ref x) = x
  show (Call f xs) = show f ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show (Lambda as s) = "(\\" ++ intercalate "," (map show as) ++ " -> " ++ show s

instance Show S where
  show (Decl a x) = "var " ++ a ++ " = " ++ show x
  show (Assign a x) = a ++ " = " ++ show x
  show (Block ss) = "{\n" ++ intercalate "\n" (map show ss) ++ "\n}"
  show (Return x) = "return " ++ show x
  show (Ctrl f xs) = "ctrl(" ++ show f ++ ")" ++ show xs

---

pr2 = Block
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [(Ref "x"), (Ref "y")])
       ]

pr1 = Block
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [(Ref "x"), (Ref "x"), (Ref "y")])
       ]

pr = Block
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "IO") [Call (Const "+") [(Ref "x"), (Ref "x"), (Ref "y")], Ref "world"])
       ]
