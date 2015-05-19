module IR where

type Name = String

data CtrlType

data E = Const String
       | Ref String
       | Call E [E]
       | Lambda [Name] S

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

data G

cmpE :: E -> E -> Bool
cmpE (Const x) (Const y) = x == y
cmpE (Ref x) (Ref y) = x == y
cmpE (Call f fs) (Call g gs) = (cmpE f g) -- && intersect
cmpE (Lambda as s) (Lambda bs t) = (cmpS s t) -- && intersect
cmpE _ _ = False

cmpS :: S -> S -> Bool
cmpS (Decl a x) (Decl b y) = a == b && cmpE x y
cmpS (Assign a x) (Assign b y) = a == b && cmpE x y
cmpS x@(Block _) y@(Block _) = genG x `cmpG` genG y
cmpS (Return x) (Return y) = cmpE x y
cmpS (Ctrl x u) (Ctrl y v) = cmpE x y && cmpS u v

genG :: S -> G
genG = undefined

cmpG :: G -> G -> Bool
cmpG = undefined

