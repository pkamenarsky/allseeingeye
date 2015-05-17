module IR where

data BinOpType

data UnaryOpType

type Name = String

data CtrlType

data St ref = LitDecl ref String
            | CallDecl ref ref [ref]
            | Ctrl CtrlType [ref] [St ref]
            | Return ref

{-
x = a + b + c

x1 = +(a, b)
x = +(x1, c)

lambdas?

Ctrl Block
  [ CallDecl "x1" "+" ["a", "b"]
  , CallDecl "x" "+" ["x1", "c"]
  ]

-}
