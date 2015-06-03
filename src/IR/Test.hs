module IR.Test where

import           Control.Monad.State

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.List
import           Data.Maybe

import           Graph
import           IR
import           IR.Serialize

pr2 = P
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [(Ref "x"), (Ref "y")])
       ]

pr1 = P
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [Ref "x", Ref "x", Ref "y", Call (Const "-") [Ref "x", Ref "y"]])
       ]

pr = P
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Assign "world" (Call (Const "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Const "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Const "IO") [Call (Const "+") [(Ref "world"), (Ref "x"), (Ref "y")], Ref "world"])
       ]

pr3_old = P
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "7")
       , Decl "writeFile" (Lambda ["name", "contents", "world"] (P [Return (Call (Ref "IO") [Ref "name", Ref "contents", Ref "world"])]))
       , Assign "world" (Call (Ref "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Ref "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Ref "IO") [Call (Ref "+") [Ref "x", Ref "y"], Ref "world"])
       ]

pr3 = P
       [ Decl "world" (Const "world")
       , Decl "a" (Const "8")
       , Decl "b" (Const "8")
       , Decl "b2" (Ref "b")
       , Decl "c" (Call (Ref "*") [Ref "a", Ref "b2"])
       , Decl "x" (Const "6")
       , Decl "y" (Const "7")
       , Decl "z" (Const "8")
       -- , Decl "writeFile" (Lambda ["name", "contents", "world"] (P [Return (Call (Ref "IO") [Ref "name", Ref "contents", Ref "world"])]))
       , Decl "writeFile" (Lambda ["name", "contents"] (P [Return (Call (Ref "IO") [Ref "name", Ref "contents", Ref "world"])]))
       , Assign "world" (Call (Ref "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Ref "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Ref "IO") [Call (Ref "+") [Ref "z", Ref "x", Ref "y"], Ref "c", Ref "world"])
       ]
