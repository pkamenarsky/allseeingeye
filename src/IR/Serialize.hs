{-# LANGUAGE OverloadedStrings #-}

module IR.Serialize where

import           Control.Applicative            ((<$>))
import           Control.Monad.ST

import           Data.Aeson
import qualified Data.HashMap.Lazy              as M
import           Data.List
import           Data.STRef
import qualified Data.Text                      as T

import           IR

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
            modifySTRef verts (M.insert xid' $ object $ [ "id" .= ("n" ++ show xid'), "size" .= (3 :: Int) ] ++ obj)
            -- modifySTRef verts (M.insert xid' $ object $ [ "id" .= ("n" ++ show xid'), "size" .= (3 :: Int), "x" .= (unsafePerformIO $ randomRIO (1, 100) :: Int), "y" .= (unsafePerformIO $ randomRIO (1, 100) :: Int) ] ++ obj)

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
                  , "edges" .= [ object [ "id"   .= ("e" ++ show eid)
                                        , "from" .= ("n" ++ show source)
                                        , "to"   .= ("n" ++ show target)
                                        ]
                               | (eid, source, target) <- edges'
                               ]
                  ]

