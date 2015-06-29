module IR.Test where

import           Control.Monad.State

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.List
import           Data.Maybe

import           Graph
import           IR
import           IR.JS
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

pr4 = Lambda ["world"] $ P
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Assign "world" (Call (Ref "f") [Ref "x", Ref "world"])
       , Assign "world" (Call (Ref "g") [Ref "y", Ref "world"])
       , Return (Ref "world")
       ]

pr5 = Lambda ["world"] $ P
       [ Decl "x" (Const "5")
       , Assign "world" (Call (Ref "f") [Ref "x", Ref "world"])
       , Decl "z" (Const "6")
       , Decl "y" (Ref "z")
       , Assign "world" (Call (Ref "g") [Ref "y", Ref "world"])
       , Return (Ref "world")
       ]

pr6 = P
       [ Decl "b" (Ref "x")
       , Decl "a" (Ref "y")
       , Assign "a" (Call (Ref "f") [Ref "a"])
       , Assign "a" (Call (Ref "f") [Ref "b"])
       , Return (Ref "a")
       ]

pr7 = P
       [ Decl "a" (Ref "y")
       , Decl "b" (Ref "x")
       , Assign "a" (Call (Ref "f") [Ref "a"])
       , Return (Ref "a")
       ]

l3 = sToP pr3

l1 = sToP pr1

l2 = sToP pr2

l = sToP pr

l4 = sToE pr4

l5 = sToE pr5

l6 = sToP pr6

l7 = sToP pr7

-- js

js1 =
  "f = function(x, y) { return x + y; }; \
  \ getf = function(f_add) { o = new Object(); o.add = f_add; return o } \
  \ console.log('log1'); \
  \ a = h; \
  \ b = h; \
  \ a++; \
  \ b++; \
  \ while(h > 5) { \
    \ console.log('log2'); \
    \ h--; \
  \ } \
  \ return getf(f).add(a, b); \
  \ }"

  -- \ a = b = ++h; \
js2 =
  "var f = function(x, y) { return x + y; }; \
  \ var a = 5; \
  \ var b = 5; \
  \ return f(a, b); \
  \ }"

js3 =
  " f = function(x, y) { return x + y; }; \
  \ getf = function(f_add) { o = new Object(); o.add = f_add; return o } \
  \ console.log('log1'); \
  \ a = h; \
  \ b = h; \
  \ a++; \
  \ b++; \
  \ return getf(f).add(a, b); \
  \ }"

jsmap =
  "{ \
  \ var map = function(f, array) { \
  \ var i = array.length; \
  \ var n = new Array(); \
  \ while (i) { \
  \   n.push_back(f(array[i])); \
  \   i = i - 1; \
  \ } \
  \ return n; \
  \ } \
  \ return map; \
  \ }"

  -- \ n.push_back(f(array[i])); \
jsmap2 =
  "{ \
  \ var i = array.length; \
  \ var n = new Array(); \
  \ n.push_back(f(array[i])); \
  \ return n; \
  \ }"

jsmap3 =
  "{ \
  \ var n = new Array(); \
  \ var i = array.length; \
  \ n.push_back(f(array[i])); \
  \ return n; \
  \ }"

jsdecl =
  " var a = 5; \
  \ var b = 666; \
  \  { \
  \    a = new Object(); \
  \    a.f = function(x) { return x + 7 }; \
  \    { \
  \      b = a.f(6); \
  \    } \
  \  } \
  \ return b; \
  \ "

jsdecl2 =
  " var a = new Class(); \
  \ var b = 666; \
  \  { \
  \    a = new Object(); \
  \    { \
  \      b = a.f(5); \
  \    } \
  \  } \
  \ return a; \
  \ "

jsio =
  " \
  \ log(x) \
  \ log(y) \
  \ "
