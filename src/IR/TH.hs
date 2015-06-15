{-# LANGUAGE TemplateHaskell #-}

module IR.TH where

import Language.Haskell.TH
import IR

lpat = do
  f  <- newName "f"
  xs <- newName "xs"
  return $ ConP 'App [VarP f, VarP xs]
