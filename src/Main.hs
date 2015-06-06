{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, FlexibleInstances, RecordWildCards, MultiParamTypeClasses, TupleSections #-}

module Main where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.ST

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.List
import qualified Data.Set                         as S
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.STRef
import           Data.Typeable

import           Language.ECMAScript3.Analysis.LexicalEnvironment

import           Language.ECMAScript3.Parser
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax

import           Graph
import           Match
import qualified IR.Test                          as IR
import qualified IR.JS                            as IRJ

import Debug.Trace

main :: IO ()
main = do
  Script _ [bs@(BlockStmt _ _), bs'@(BlockStmt _ _)] <- parseFromFile "test.js"
  let ss = const() <$> bs
  print ss
  return ()

pparse :: IO ()
pparse = do
  prs <- parseFromFile "test.js"
  print $ const () <$> prs
