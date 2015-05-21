{-# LANGUAGE MultiParamTypeClasses #-}

module Graph where

import           Control.Applicative              ((<$>))
import           Control.Monad
import           Control.Monad.ST

import qualified Data.Map                         as M
import           Data.STRef

class Graph n a where
  node :: n -> (a, [n])

topsort :: (Ord a, Ord (n a), Graph (n a) b) => n a -> [b]
topsort root = runST $ do
  visited <- newSTRef M.empty
  stack   <- newSTRef []

  let f n = do
        v <- M.findWithDefault False n <$> readSTRef visited
        unless v $ do
          modifySTRef visited $ M.insert n True
          let (a, ns) = node n
          forM ns f
          modifySTRef stack (a:)

  f root
  readSTRef stack

