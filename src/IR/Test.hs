module IR.Test where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B

import           IR
import           IR.Serialize

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

nodes = "var nodes = " ++ (B.unpack $ encode $ serializeG $ fst $ genG ectx pr1)
