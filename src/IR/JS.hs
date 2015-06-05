module IR.JS where

import           Language.ECMAScript3.Syntax

import           IR

setA :: L -> Int -> L -> L
setA arr i e = (App (App (App (Var "setA") arr) (Cnst $ show i)) e)

convert :: Expression a -> L -> L
convert (StringLit a lit) cnt = Cnst lit
{-
convert (RegexpLit a lit glb csi) = Cnst lit
convert (NumLit a lit) = Cnst $ show lit
convert (IntLit a lit) = Cnst $ show lit
convert (BoolLit a lit) = Cnst $ show lit
convert (NullLit a) = Var "null"
convert (ArrayLit a es) = App (Lam "array" ((setA (Var "array") 0 (convert $ head es))))
                              (App (Var "newArray") (Var "world"))

convert (ObjectLit a [(Prop a, Expression a)])
convert (ThisRef a)
convert (VarRef a (Id a))
convert (DotRef a (Expression a) (Id a))
convert (BracketRef a (Expression a) {- container -} (Expression a) {- key -})
convert (NewExpr a (Expression a) {- constructor -} [Expression a])
convert (PrefixExpr a PrefixOp (Expression a))
convert (UnaryAssignExpr a UnaryAssignOp (LValue a))
convert (InfixExpr a InfixOp (Expression a) (Expression a))
convert (CondExpr a (Expression a) (Expression a) (Expression a))
-}
convert (AssignExpr a op (LVar a' lv) e) cnt = let cnt' = App (Lam lv cnt) (convert e cnt') in cnt'
convert (ListExpr a es) cnt = foldr convert cnt es
{-
convert (CallExpr a (Expression a) [Expression a])
convert (FuncExpr a (Maybe (Id a)) [Id a] [Statement a])
-}
