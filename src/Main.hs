{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Arrow
import           Control.Applicative

import qualified Data.Set                         as S
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Data
import           Data.Typeable

import           Language.ECMAScript3.Analysis.LexicalEnvironment

import           Language.ECMAScript3.Parser
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax

import           Match

-- multitree
data Strand a = Strand (Statement a) [Strand a] deriving (Eq, Ord, Show)

data RefHead a = RHVar a String | RHDot a (Expression a) String | RHBracket a (Expression a) (Expression a) deriving (Eq, Ord, Show)

lValueToRefHead :: LValue a -> RefHead a
lValueToRefHead (LVar a v)        = RHVar a v
lValueToRefHead (LDot a e v)      = RHDot a e v
lValueToRefHead (LBracket a e e') = RHBracket a e e'

idToRefHead :: Id a -> RefHead a
idToRefHead (Id a v) = RHVar a v

data Context a = Context
  { ctxRefHeads :: M.Map (RefHead a) (Strand a)
  , ctxStrands  :: [Strand a]
  } deriving Show

emptyCtx :: Context a
emptyCtx = Context M.empty []

getRefHead :: Ord a => Context a -> RefHead a -> Maybe (Strand a)
getRefHead (Context {..}) vid = M.lookup vid ctxRefHeads

setRefHead :: Ord a => RefHead a -> Statement a -> Context a -> Context a
setRefHead vid st ctx@(Context {..}) = ctx { ctxRefHeads = M.alter f vid ctxRefHeads }
  where
    f Nothing    = Just $ Strand st []
    f (Just str) = Just $ Strand st [str]

walk' :: JavaScript a -> (Statement a -> st -> st) -> st -> st
walk' = undefined

walk :: Ord a => Context a -> Statement a -> Context a
walk ctx (BlockStmt _ sts) = foldl walk ctx sts
walk ctx st@(VarDeclStmt _ decls) = foldr (\(VarDecl _ vid _) -> setRefHead (idToRefHead vid) st) ctx decls
walk ctx st@(ExprStmt _ (AssignExpr _ _ lv _)) = setRefHead (lValueToRefHead lv) st ctx
walk ctx st@(ExprStmt _ (UnaryAssignExpr _ _ lv)) = setRefHead (lValueToRefHead lv) st ctx
{-
walk ctx st@(ExprStmt _ (VarRef _ vid)) = fromMaybe ctx $ addDep <$> dep <*> pure st <*> pure ctx
  where
    dep = getVarDecl ctx vid
-}
walk ctx _ = ctx

main :: IO ()
main = do
  Script _ [st@(BlockStmt _ _)] <- parseFromFile "test.js"
  print $ walk emptyCtx (const () <$> st)

