{-# LANGUAGE OverloadedStrings #-}

module IR.Serialize where

import           Control.Applicative            ((<$>))
import           Control.Monad
import           Control.Monad.ST

import           Data.Aeson
import qualified Data.HashMap.Lazy              as M
import           Data.List
import           Data.Maybe
import           Data.STRef
import qualified Data.Text                      as T

import           IR

instance Show E where
  show (Const x) = x
  show (Ref x) = x
  show (Call f xs) = show f ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show (Lambda as s) = "\\(" ++ intercalate "," as ++ ") -> " ++ show s

instance Show S where
  show (Decl a x) = "var " ++ a ++ " = " ++ show x
  show (Assign a x) = a ++ " = " ++ show x
  show (Block (P ss)) = "{\n" ++ intercalate "\n" (map show ss) ++ "\n}"
  show (Return x) = "return " ++ show x
  show (Ctrl f xs) = "ctrl(" ++ show f ++ ")" ++ show xs

serializeG :: G a -> Value
serializeG g = runST $ do
  nid   <- newSTRef (0 :: Int)
  eid   <- newSTRef (0 :: Int)
  nmap  <- newSTRef M.empty
  verts <- newSTRef M.empty
  edges <- newSTRef []

  let insNode x obj f = do
        xid <- case x of
          Just x' -> M.lookup x' <$> readSTRef nmap
          Nothing -> return Nothing

        case xid of
          Just xid' -> return xid'
          Nothing   -> do
            xid' <- readSTRef nid

            modifySTRef nid (+1)
            modifySTRef verts (M.insert xid' $ object $ [ "id" .= ("n" ++ show xid'), "size" .= (3 :: Int) ] ++ obj)

            f xid'

            case x of
              Just x' -> modifySTRef nmap (M.insert x' xid')
              Nothing -> return ()

            return xid'

      insEdges fromId toG = do
        toIds <- nub <$> mapM ser toG

        forM toIds $ \toId -> do
          eid' <- readSTRef eid
          modifySTRef eid (+1)
          modifySTRef edges ((eid', fromId, toId):)

        return toIds

      ser (G_Const c) = insNode Nothing [ "label" .= ("const " ++ c) ] $ \_ -> return ()
      ser (G_ExtRef x) = insNode (Just x) [ "label" .= ("extrn " ++ x) ] $ \_ -> return ()
      ser (G_Call f xs) = do
        let isRef (G_ExtRef n) = Just n
            isRef _            = Nothing
        insNode Nothing [ "shape" .= ("box" :: T.Text), "label" .= (fromMaybe "call" (isRef f)) ] $ \xid -> do
          if isJust $ isRef f
            then insEdges xid xs
            else insEdges xid (f:xs)
      ser (G_Lambda ns f) = do
        -- insNode Nothing [ "label" .= ("\\" ++ (intercalate "," ns) ++ " ->") ] $ \xid -> insEdges xid [f]
        insNode Nothing [ "label" .= ("\\" :: String ) ] $ \xid -> insEdges xid [f]
      ser (G_Decl x s) = do
        insNode (Just $ "var " ++ x) [ "label" .= ("var " ++ x) ] $ \xid -> insEdges xid [s]
      ser (G_Arg x) = do
        insNode (Just $ "arg " ++ x) [ "label" .= ("arg " ++ x) ] $ \_ -> return ()
      ser (G_Assign x s) = do
        insNode (Just $ x ++ " =") [ "label" .= (x ++ " =") ] $ \xid -> insEdges xid [s]
      ser (G_Return x) = do
        insNode Nothing [ "shape" .= ("dot" :: T.Text), "label" .= ("" :: String) ] $ \xid -> insEdges xid [x]
      ser (G_Ctrl x y) = do
        insNode Nothing [ "label" .= ("ctrl" :: String) ] $ \xid -> do
          insEdges xid [x]
          insEdges xid [y]
      ser (G_Nop) = insNode Nothing [ "label" .= ("nop" :: String) ] $ \_ ->  return ()

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

serializeE :: G a -> [(Int, Int)]
serializeE g = runST $ do
  nid   <- newSTRef (0 :: Int)
  eid   <- newSTRef (0 :: Int)
  nmap  <- newSTRef M.empty
  verts <- newSTRef M.empty
  edges <- newSTRef []

  let insNode x obj f = do
        xid <- case x of
          Just x' -> M.lookup x' <$> readSTRef nmap
          Nothing -> return Nothing

        case xid of
          Just xid' -> return xid'
          Nothing   -> do
            xid' <- readSTRef nid

            modifySTRef nid (+1)
            modifySTRef verts (M.insert xid' $ object $ [ "id" .= ("n" ++ show xid'), "size" .= (3 :: Int) ] ++ obj)

            f xid'

            case x of
              Just x' -> modifySTRef nmap (M.insert x' xid')
              Nothing -> return ()

            return xid'

      insEdges fromId toG = do
        toIds <- nub <$> mapM ser toG

        forM toIds $ \toId -> do
          eid' <- readSTRef eid
          modifySTRef eid (+1)
          modifySTRef edges ((fromId, toId):)

        return toIds

      ser (G_Const c) = insNode Nothing [ "label" .= ("const " ++ c) ] $ \_ -> return ()
      ser (G_ExtRef x) = insNode (Just x) [ "label" .= ("extrn " ++ x) ] $ \_ -> return ()
      ser (G_Call f xs) = do
        let isRef (G_ExtRef n) = Just n
            isRef _            = Nothing
        insNode Nothing [ "shape" .= ("box" :: T.Text), "label" .= (fromMaybe "call" (isRef f)) ] $ \xid -> do
          if isJust $ isRef f
            then insEdges xid xs
            else insEdges xid (f:xs)
      ser (G_Lambda ns f) = do
        -- insNode Nothing [ "label" .= ("\\" ++ (intercalate "," ns) ++ " ->") ] $ \xid -> insEdges xid [f]
        insNode Nothing [ "label" .= ("\\" :: String ) ] $ \xid -> insEdges xid [f]
      ser (G_Decl x s) = do
        insNode (Just $ "var " ++ x) [ "label" .= ("var " ++ x) ] $ \xid -> insEdges xid [s]
      ser (G_Arg x) = do
        insNode (Just $ "arg " ++ x) [ "label" .= ("arg " ++ x) ] $ \_ -> return ()
      ser (G_Assign x s) = do
        insNode (Just $ x ++ " =") [ "label" .= (x ++ " =") ] $ \xid -> insEdges xid [s]
      ser (G_Return x) = do
        insNode Nothing [ "shape" .= ("dot" :: T.Text), "label" .= ("" :: String) ] $ \xid -> insEdges xid [x]
      ser (G_Ctrl x y) = do
        insNode Nothing [ "label" .= ("ctrl" :: String) ] $ \xid -> do
          insEdges xid [x]
          insEdges xid [y]
      ser (G_Nop) = insNode Nothing [ "label" .= ("nop" :: String) ] $ \_ ->  return ()

  ser g

  verts' <- readSTRef verts
  edges' <- readSTRef edges

  return edges'

instance ToJSON Label where
  toJSON (L_Const n)    = toJSON $ "const " ++ n
  toJSON (L_ExtRef n)   = toJSON $ "extref " ++ n
  toJSON (L_ClosureRef) = toJSON ("closureref" :: String)
  toJSON (L_Call)       = toJSON ("call" :: String)
  toJSON (L_Lambda ns)  = toJSON ("\\" :: String)
  toJSON (L_Decl n)     = toJSON $ "decl " ++ n
  toJSON (L_Arg n)      = toJSON $ "arg " ++ n
  toJSON (L_Assign n)   = toJSON $ n ++ " ="
  toJSON (L_Return)     = toJSON ("return" :: String)
  toJSON (L_Ctrl)       = toJSON ("ctrl" :: String)
  toJSON (L_Nop)        = toJSON ("nop" :: String)

instance (ToJSON l, Show l) => ToJSON (G2 l a) where
  toJSON (G2 _ ns es) = object
    [ "nodes" .= [ object [ "id"    .= ("n" ++ show nid)
                          , "label" .= (show nid ++ ": " ++ show l)
                          -- , "label" .= toJSON l
                          ]
                 | (nid, l) <- ns
                 ]
    , "edges" .= [ object [ "from" .= ("n" ++ show e1), "to" .= ("n" ++ show e2) ]
                 | (e1, e2) <- es
                 ]
    ]
