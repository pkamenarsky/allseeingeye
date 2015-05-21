module IR.Test where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.List

import           Graph
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
       , Return (Call (Const "+") [Ref "x", Ref "x", Ref "y", Call (Const "-") [Ref "x", Ref "y"]])
       ]

pr = Block
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Assign "world" (Call (Const "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Const "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Const "IO") [Call (Const "+") [(Ref "world"), (Ref "x"), (Ref "y")], Ref "world"])
       ]

pr3 = Block
       [ Decl "world" (Const "world")
       , Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Assign "world" (Call (Const "rand") [Ref "x", Ref "world"])
       , Assign "world" (Call (Const "writeFile") [Ref "y", Ref "x", Ref "world"])
       , Return (Call (Const "IO") [Call (Const "+") [(Ref "x"), (Ref "y")], Ref "world"])
       ]

getLocal x = Call (Const $ "local[" ++ x ++ "]") [Ref "locals"]

pr4 = Block
       [ Decl "locals" (Const "locals + world")
       , Assign "locals" (Call (Const "rand") [getLocal "x", getLocal "world"])
       , Assign "locals" (Call (Const "writeFile") [getLocal "y", getLocal "x", getLocal "world"])
       , Return (Call (Const "IO") [Call (Const "+") [getLocal "x", getLocal "y"], getLocal "world"])
       ]

g :: G ()
g = fst $ genG ectx pr3

nodes = "var nodes = " ++ (B.unpack $ encode $ serializeG g)

ts :: [String]
ts = topsort g

pts = intercalate " -> " ts