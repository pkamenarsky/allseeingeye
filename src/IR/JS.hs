module IR.JS where

import           Data.Generics.Str
import           Data.Generics.Uniplate.Data
import           Data.Data

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

import           IR

setA :: L -> Int -> L -> L
setA arr i e = (App (App (App (Var "setA") arr) (Cnst $ show i)) e)

{-
convert2 :: [Expression a] -> L
convert2 [(StringLit a lit)] = Cnst lit
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
convert2 [(VarRef a (Id a' ref))] cnt = Var ref
{-
convert (DotRef a (Expression a) (Id a))
convert (BracketRef a (Expression a) {- container -} (Expression a) {- key -})
convert (NewExpr a (Expression a) {- constructor -} [Expression a])
convert (PrefixExpr a PrefixOp (Expression a))
-}
convert2 (UnaryAssignExpr a PrefixInc (LVar a' lv)) cnt = App (Lam lv cnt) ((Var lv))
{-
convert (InfixExpr a InfixOp (Expression a) (Expression a))
convert (CondExpr a (Expression a) (Expression a) (Expression a))
-}
convert2 (AssignExpr a op (LVar a' lv) e) cnt =
  let cnt' = convert e cnt in App (Lam lv cnt') cnt'
convert2 (ListExpr a es) cnt = foldr convert cnt es
{-
convert (CallExpr a (Expression a) [Expression a])
convert (FuncExpr a (Maybe (Id a)) [Id a] [Statement a])
-}
-}

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
convert (UnaryAssignExpr a PrefixInc (LVar a' lv)) cnt = App (Lam lv cnt) ((Var lv))
{-
convert (InfixExpr a InfixOp (Expression a) (Expression a))
convert (CondExpr a (Expression a) (Expression a) (Expression a))
-}
convert (AssignExpr a op (LVar a' lv) e) cnt =
  let cnt' = App (Lam lv cnt') (convert e cnt') in cnt'
convert (ListExpr a es) cnt = foldr convert cnt es
{-
convert (CallExpr a (Expression a) [Expression a])
convert (FuncExpr a (Maybe (Id a)) [Id a] [Statement a])
-}

unnestAssigns :: Data a => Expression a -> Expression a
unnestAssigns e | null asgns = e
                | otherwise  = ListExpr (getAnnotation e) (map snd asgns ++ [e'])
  where (ch, f) = uniplate e
        (ss, s) = strStructure ch
        asgns = [ (VarRef a' (Id a' lv), ass) | ass@(AssignExpr a op (LVar a' lv) e) <- ss ]
        e' = f $ s (map fst asgns)

testExpr = (ListExpr () [AssignExpr () OpAssign (LVar () "b") (UnaryAssignExpr () PrefixInc (LVar () "x")),AssignExpr () OpAssign (LVar () "c") (VarRef () (Id () "y"))])
testExpr2 = (ListExpr () [AssignExpr () OpAssign (LVar () "b") (AssignExpr () OpAssign (LVar () "x") (VarRef () (Id () "z"))),AssignExpr () OpAssign (LVar () "c") (VarRef () (Id () "y"))])

testConvert = convert testExpr (Lam "xxx" (Var "xxx"))
testConvert2 = convert testExpr2 (Lam "xxx" (Var "xxx"))

