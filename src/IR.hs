module IR where

type Name = String

data CtrlType

data E ref = Const String
           | Ref ref
           | Call (E ref) [E ref]
           | Lambda [Name] [S ref]

data S ref = Decl Name (E ref)
           | Assign Name (E ref)
           | Block [S ref]
           | Ctrl (E ref) (S ref)

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
