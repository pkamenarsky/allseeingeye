{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main where

import           Control.Arrow
import           Control.Applicative

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.List
import qualified Data.Set                         as S
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Typeable

import           Language.ECMAScript3.Analysis.LexicalEnvironment

import           Language.ECMAScript3.Parser
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax

import           Match

-- multitree
data Strand a = Strand (Statement a) [Strand a] deriving (Eq, Ord, Show)

-- FIXME: remove double references, i.e. a -> b -> c should disallow a -> c

-- FIXME: we can not use RHDot and RHBracked, *except* when the expressions are literals
data Ref a = RHVar a String | RHDot a (Expression a) String | RHBracket a (Expression a) (Expression a) deriving (Eq, Ord, Show)

lValueToRef :: LValue a -> Ref a
lValueToRef (LVar a v)        = RHVar a v
lValueToRef (LDot a e v)      = RHDot a e v
lValueToRef (LBracket a e e') = RHBracket a e e'

idToRef :: Id a -> Ref a
idToRef (Id a v) = RHVar a v

data Context a = Context
  { ctxRefHeads :: M.Map (Ref a) (Strand a)
  , ctxStrands  :: M.Map (Ref a) (Strand a)
  } deriving Show

emptyCtx :: Context a
emptyCtx = Context M.empty M.empty

getRefHead :: Ord a => Context a -> Ref a -> Maybe (Strand a)
getRefHead (Context {..}) vid = M.lookup vid ctxRefHeads

extractRefs :: Data a => Expression a -> [Ref a]
extractRefs = para f
  where
    f (VarRef a (Id _ vid)) vids   = RHVar a vid : concat vids
    {-
    f (DotRef a e (Id _ vid)) vids = RHDot a e vid : concat vids
    f (BracketRef a e e') vids     = RHBracket a e e' : concat vids
    -}
    f _ vids                       = concat vids

setRefHead :: (Data a, Ord a) => Ref a -> Statement a -> Context a -> Context a
setRefHead vid st ctx@(Context {..}) = ctx { ctxRefHeads = M.alter f vid ctxRefHeads }
  where
    f Nothing    = Just $ Strand st ([]    ++ refs)
    f (Just str) = Just $ Strand st ([str] ++ refs)

    refs = mapMaybe (getRefHead ctx) (stRefs st)

    stRefs (VarDeclStmt _ decls)
      = concatMap (\(VarDecl _ _ e) -> maybe [] extractRefs e) decls
    stRefs (ExprStmt _ (AssignExpr _ _ _ e)) = extractRefs e
    stRefs _                                 = []

addStrand :: (Data a, Ord a) => Statement a -> Context a -> Context a
addStrand st@(ExprStmt _ e) ctx = ctx { ctxStrands = foldr (uncurry M.insert) (ctxStrands ctx) refs }
  where
    refs = mapMaybe (\e -> (e,) <$> (f $ getRefHead ctx e)) (extractRefs e)

    f Nothing    = Nothing
    f (Just str) = Just $ Strand st [str]

addStrand _ ctx = ctx

walk' :: JavaScript a -> (Statement a -> st -> st) -> st -> st
walk' = undefined

walk :: (Ord a, Data a) => Context a -> Statement a -> Context a
walk ctx (BlockStmt _ sts) = foldl walk ctx sts
walk ctx st@(VarDeclStmt _ decls) = addStrand st $ foldr (\(VarDecl _ vid _) -> setRefHead (idToRef vid) st) ctx decls
walk ctx st@(ExprStmt _ (AssignExpr _ _ lv _)) = addStrand st $ setRefHead (lValueToRef lv) st ctx
walk ctx st@(ExprStmt _ (UnaryAssignExpr _ _ lv)) = addStrand st $ setRefHead (lValueToRef lv) st ctx
walk ctx st = addStrand st ctx

main :: IO ()
main = do
  Script _ [st@(BlockStmt _ _)] <- parseFromFile "test.js"
  printContext $ walk emptyCtx (const () <$> st)

pparse :: IO ()
pparse = do
  prs <- parseFromFile "test.js"
  print $ const () <$> prs

printStrand :: Int -> Strand a -> String
printStrand level (Strand st strs) = (replicate level ' ') ++ (show $ prettyPrint st) ++ "\n" ++ (intercalate "\n" $ map (printStrand (level + 2)) strs)

printContext :: Context a -> IO ()
printContext (Context {..}) = putStrLn $ intercalate "\n" $ map (printStrand 0) (M.elems ctxStrands)
