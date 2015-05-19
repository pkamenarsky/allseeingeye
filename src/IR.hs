module IR where

type Name = String

data CtrlType

data E = Const String
       | Ref String
       | Call E [E]
       | Lambda [Name] [S]

data S = Decl Name E
       | Assign Name E
       | Block [S]
       | Return E
       | Ctrl E S -- merge with block?

pr = Block
       [ Decl "x" (Const "5")
       , Decl "y" (Const "6")
       , Return (Call (Const "+") [(Ref "x"), (Ref "y")])
       ]

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
