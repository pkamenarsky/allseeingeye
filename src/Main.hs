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

main :: IO ()
main = do
  Script _ parsed <- parseFromFile "test.js"
  -- print $ (const ()) `fmap` parsed
  print $ localVars parsed
  --print $ prettyPrint parsed

idEq :: Id a -> Id a -> Bool
idEq (Id _ i) (Id _ i') = i == i'

expDep :: Expression a -> Expression a -> [Id a]
expDep (ArrayLit _ es) x = concatMap (flip expDep x) es
-- expDep (ObjectLit _ props) x = concatMap (flip expDep x) es

isDep :: Expression a -> Statement a -> [Id a]
isDep e (BlockStmt _ ss) = concatMap (isDep e) ss
isDep e (EmptyStmt _) = []
-- ...
isDep (VarRef _ rid) (VarDeclStmt _ vds) = concatMap (varDep rid) vds
  where
    varDep rid' (VarDecl _ vid _) | idEq rid' vid = [vid]
                                  | otherwise     = []

d1 = [VarDeclStmt () [VarDecl () (Id () "a") (Just (IntLit () 4))],ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "alert")) [StringLit () "THIS IS A TEST"]),ReturnStmt () (Just (InfixExpr () OpAdd (VarRef () (Id () "a")) (IntLit () 1)))]
d2 = [ExprStmt () (CallExpr () (DotRef () (VarRef () (Id () "console")) (Id () "alert")) [StringLit () "THIS IS A TEST"]),VarDeclStmt () [VarDecl () (Id () "a") (Just (IntLit () 4))],ReturnStmt () (Just (InfixExpr () OpAdd (VarRef () (Id () "a")) (IntLit () 1)))]

-- multitree
data Strand a = Strand (Statement a) [Strand a] deriving (Eq, Ord, Show)

data IdOrLValue a = IId (Id a) | ILValue (LValue a) deriving (Eq, Ord, Show)

data Context a = Context
  { ctxRefHeads :: M.Map (IdOrLValue a) (Strand a)
  , ctxStrands  :: [Strand a]
  } deriving Show

emptyCtx :: Context a
emptyCtx = Context M.empty []

getRefHead :: Ord a => Context a -> IdOrLValue a -> Maybe (Strand a)
getRefHead (Context {..}) vid = M.lookup vid ctxRefHeads

addRefHead :: Ord a => IdOrLValue a -> Statement a -> Context a -> Context a
addRefHead vid st ctx@(Context {..}) = undefined

walk' :: JavaScript a -> (Statement a -> st -> st) -> st -> st
walk' = undefined

walk :: Ord a => Context a -> Statement a -> Context a
walk ctx (BlockStmt _ sts) = foldl walk ctx sts
walk ctx st@(VarDeclStmt _ decls) = foldr (\(VarDecl _ vid _) -> addRefHead (IId vid) st) ctx decls
{-
walk ctx st@(ExprStmt _ (VarRef _ vid)) = fromMaybe ctx $ addDep <$> dep <*> pure st <*> pure ctx
  where
    dep = getVarDecl ctx vid
-}
walk ctx _ = ctx
