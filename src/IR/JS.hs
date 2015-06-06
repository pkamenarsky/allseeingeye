module IR.JS where

import           Control.Arrow
import           Control.Monad.State

import           Data.Generics.Str
import           Data.Generics.Uniplate.Data
import           Data.Data
import           Data.Maybe

import           Language.ECMAScript3.Parser
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

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
-}
convert (VarRef a (Id a' ref)) cnt = Var ref
{-
convert (DotRef a (Expression a) (Id a))
convert (BracketRef a (Expression a) {- container -} (Expression a) {- key -})
convert (NewExpr a (Expression a) {- constructor -} [Expression a])
convert (PrefixExpr a PrefixOp (Expression a))
-}
convert (UnaryAssignExpr a PrefixInc (LVar a' lv)) cnt =
  App (Lam lv (App (Lam lv cnt) (App (Var "inc") (Var lv)))) (App (Var "inc") (Var lv))
convert (UnaryAssignExpr a PostfixInc (LVar a' lv)) cnt =
  (App (Lam lv cnt) (App (Var "inc") (Var lv)))
{-
convert (InfixExpr a InfixOp (Expression a) (Expression a))
convert (CondExpr a (Expression a) (Expression a) (Expression a))
-}
convert (AssignExpr a op (LVar a' lv) e) cnt =
  let cnt' = App (Lam lv cnt) (convert e cnt') in cnt'
convert (ListExpr a es) cnt = foldr convert cnt es
{-
convert (CallExpr a (Expression a) [Expression a])
convert (FuncExpr a (Maybe (Id a)) [Id a] [Statement a])
-}

unnestAssigns :: Data a => Expression a -> ([Expression a], Expression a, [Expression a])
unnestAssigns e = case e of
  e1@(AssignExpr a op (LVar a1 lv) (VarRef a2 ref)) ->
    ([e1], VarRef a1 (Id a1 lv), [])
  e1@(AssignExpr a op (LVar a1 lv) e2) -> let (pre, e3, post) = unnestAssigns e2 in
    (pre ++ [AssignExpr a op (LVar a1 lv) e3], VarRef a1 (Id a1 lv), [])

unnest :: Data a => Expression a -> Expression a
unnest e = ListExpr (getAnnotation e) (pre ++ [e'] ++ post)
  where (pre, e', post) = unnestAssigns e

testExpr = case parse expression "" "x = y = z = a = b" of
  Right expr -> expr
  Left err   -> error $ show err
testExpr2 = (ListExpr () [AssignExpr () OpAssign (LVar () "b") (AssignExpr () OpAssign (LVar () "x") (VarRef () (Id () "z"))),AssignExpr () OpAssign (LVar () "c") (VarRef () (Id () "y"))])

testConvert = convert (unnest testExpr) (Lam "xxx" (Var "xxx"))
testConvert2 = convert (unnest testExpr2) (Lam "xxx" (Var "xxx"))

