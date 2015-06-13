module IR.JS where

import           Control.Arrow
import           Control.Monad.State

import           Data.Generics.Str
import           Data.Generics.Uniplate.Data
import           Data.Data
import qualified Data.Map                                 as M
import           Data.Maybe

import           Language.ECMAScript3.Parser
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

import           IR

-- function name, impure arguments (0 is this, -1 is world)
fns :: M.Map String [Int]
fns = M.fromList [ ( "push", [0] ) ]

convert :: Expression a -> State ([S] -> [S]) E
convert (StringLit a lit) = return $ Const lit
convert (RegexpLit a lit glb csi) = return $ Const lit
convert (NumLit a lit) = return $ Const $ show lit
convert (IntLit a lit) = return $ Const $ show lit
convert (BoolLit a lit) = return $ Const $ show lit
convert (NullLit a) = return $ Ref "null"
{-
convert (ArrayLit a es) = App (Lam "array" ((setA (Var "array") 0 (convert $ head es))))
                              (App (Var "newArray") (Var "world"))

convert (ObjectLit a [(Prop a, Expression a)])
convert (ThisRef a)
-}
convert (VarRef a (Id a' ref)) = return $ Ref ref
convert (DotRef _ e (Id _ ref)) = do
  e' <- convert e
  return $ Call (Ref "get") [e', Const ref]
{-
convert (BracketRef a (Expression a) {- container -} (Expression a) {- key -})
convert (NewExpr a (Expression a) {- constructor -} [Expression a])
convert (PrefixExpr a PrefixOp (Expression a))
-}
convert (UnaryAssignExpr a op (LVar a' lv))
  | (opname, True)  <- op' = do
      modify (\f -> \cnt -> f [Assign lv (Call (Ref opname) [Ref lv])] ++ cnt)
      return $ Ref lv
  | (opname, False) <- op' = do
      modify (\f -> \cnt -> f cnt ++ [Assign lv (Call (Ref opname) [Ref lv])])
      return $ Ref lv
  where
    op' = getOp op
    getOp PrefixInc  = ("inc", True )
    getOp PostfixInc = ("inc", False)
    getOp PrefixDec  = ("dec", True )
    getOp PostfixDec = ("dec", False)
{-
convert (InfixExpr a InfixOp (Expression a) (Expression a))
convert (CondExpr a (Expression a) (Expression a) (Expression a))
-}
convert (AssignExpr a op (LVar a' lv) e) = do
  e' <- convert e
  -- modify (++ [Assign lv e'])
  modify (\f -> \cnt -> f [Assign lv e'] ++ cnt)
  return $ Ref lv
{-
convert (ListExpr a es) | null es   = cnt
                        | otherwise = foldr convert (filter (not . isRef) (init es) ++ [last es])
  -- we need to filter all useless refs inside the list, like "y" in
  -- x++, y, z
  where isRef (VarRef _ _)       = True
        isRef (DotRef _ _ _)     = True
        isRef (BracketRef _ _ _) = True
        isRef (ThisRef _)        = True
        isRef _                  = False
convert (CallExpr a f xs)
  -- FIXME: what to do about the undefineds here?
  | Just n <- getName f, Just df <- M.lookup n fns
    = if length df == 1
      -- then App (Lam (convert (xs !! (df !! 0)) undefined) cnt) expr
      then App (Lam "a" cnt) expr
      else undefined
  | otherwise = foldl App (convert f undefined) (map (flip convert undefined) xs)
    where getName (VarRef _ (Id _ ref))   = Just ref
          getName (DotRef _ _ (Id _ ref)) = Just ref
          getName _                       = Nothing
          expr = foldl App (convert f undefined) (map (flip convert undefined) xs)
convert (FuncExpr a n xs ss) = undefined
-}

parseExpr str = case parse expression "" str of
  Right expr -> expr
  Left err   -> error $ show err

testConvert :: String -> P
testConvert e = P $ ss [Return e']
  where (e', ss) = runState (convert $ parseExpr e) id

texpr1 = parseExpr "a.exec('fn').push(b), noobj('arg'), a"

texpr2 = parseExpr "b = x, a = y, a = f(b), a = f(a), a = f(a), a = f(a), a"

texpr3 = parseExpr "a = y, b = x, a = f(b), a = f(a), a = f(a), a"
