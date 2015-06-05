module IR.JS where

import           Language.ECMAScript3.Syntax

import           IR

convert :: Expression a -> L
convert (StringLit a lit) = Cnst lit
convert (RegexpLit a lit glb csi) = Cnst lit
convert (NumLit a lit) = Cnst $ show lit
convert (IntLit a lit) = Cnst $ show lit
convert (BoolLit a lit) = Cnst $ show lit
{-
convert (NullLit a)
convert (ArrayLit a [Expression a])
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
convert (AssignExpr a AssignOp (LValue a) (Expression a))
convert (ListExpr a [Expression a])
convert (CallExpr a (Expression a) [Expression a])
convert (FuncExpr a (Maybe (Id a)) [Id a] [Statement a])
-}
