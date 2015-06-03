module IR.Test where

import           Control.Monad.State

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.List
import           Data.Maybe

import           Graph
import           IR
import           IR.Serialize

pr2 = Block $ P
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [(Ref "x"), (Ref "y")])
       ]

pr1 = Block $ P
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [Ref "x", Ref "x", Ref "y", Call (Const "-") [Ref "x", Ref "y"]])
       ]

pr = Block $ P
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Assign "world" (Call (Const "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Const "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Const "IO") [Call (Const "+") [(Ref "world"), (Ref "x"), (Ref "y")], Ref "world"])
       ]

pr3_old = Block $ P
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "7")
       , Decl "writeFile" (Lambda ["name", "contents", "world"] (Block $ P [Return (Call (Ref "IO") [Ref "name", Ref "contents", Ref "world"])]))
       , Assign "world" (Call (Ref "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Ref "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Ref "IO") [Call (Ref "+") [Ref "x", Ref "y"], Ref "world"])
       ]

pr3 = Block $ P
       [ Decl "world" (Const "world")
       , Decl "a" (Const "8")
       , Decl "b" (Const "8")
       , Decl "b2" (Ref "b")
       , Decl "c" (Call (Ref "*") [Ref "a", Ref "b2"])
       , Decl "x" (Const "6")
       , Decl "y" (Const "7")
       , Decl "z" (Const "8")
       -- , Decl "writeFile" (Lambda ["name", "contents", "world"] (Block $ P [Return (Call (Ref "IO") [Ref "name", Ref "contents", Ref "world"])]))
       , Decl "writeFile" (Lambda ["name", "contents"] (Block $ P [Return (Call (Ref "IO") [Ref "name", Ref "contents", Ref "world"])]))
       , Assign "world" (Call (Ref "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Ref "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Ref "IO") [Call (Ref "+") [Ref "z", Ref "x", Ref "y"], Ref "c", Ref "world"])
       ]

getLocal x = Call (Const $ "local[" ++ x ++ "]") [Ref "locals"]

pr4 = Block $ P
       [ Decl "locals" (Const "locals + world")
       , Assign "locals" (Call (Const "rand") [getLocal "x", getLocal "world"])
       , Assign "locals" (Call (Const "writeFile") [getLocal "y", getLocal "x", getLocal "world"])
       , Return (Call (Const "IO") [Call (Const "+") [getLocal "x", getLocal "y"], getLocal "world"])
       ]

g :: G2 Label ()
g = execState (genG2fromS ectx2 pr3) emptyG2

-- ge = removeSubgraph 16 g
ge = removeOrphanDecls g
-- ge = g

nodes = "var nodes = " ++ (B.unpack $ encode ge)

{-
g :: G ()
g = fromMaybe G_Nop $ inlineLambdas $ fst $ genG ectx pr3_old

nodes = "var nodes = " ++ (B.unpack $ encode $ serializeG g)

ts :: [String]
ts = topsort g

pts = intercalate " -> " ts
-}
