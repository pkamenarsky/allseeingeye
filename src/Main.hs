module Main where

import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax

main :: IO ()
main = do
  parsed <- parseFromFile "test.js"
  print $ (const ()) `fmap` parsed
  --print $ prettyPrint parsed

idEq :: Id a -> Id a -> Bool
idEq (Id _ i) (Id _ i') = i == i'

expDep :: Expression a -> Expression a -> [Id a]
expDep (ArrayLit _ es) x = concatMap (flip expDep x) es

isDep :: Expression a -> Statement a -> [Id a]
isDep e (BlockStmt _ ss) = concatMap (isDep e) ss
isDep e (EmptyStmt _) = []
-- ...
isDep (VarRef _ rid) (VarDeclStmt _ vds) = concatMap (varDep rid) vds
  where
    varDep rid' (VarDecl _ vid _) | idEq rid' vid = [vid]
                                  | otherwise     = []
