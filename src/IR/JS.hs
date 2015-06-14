module IR.JS where

import           Control.Applicative                      ((<$>))
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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip (maybe (return ()))

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
convert (BracketRef a e i) = do
  e' <- convert e
  i' <- convert i
  return $ Call (Ref "get_elem") [e', i']
convert (NewExpr a f xs) = do
  f'  <- convert f
  xs' <- mapM convert xs
  return $ Call f' xs'
convert (PrefixExpr a op e) = do
  e' <- convert e
  return $ Call (Ref $ show op) [e']
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
convert (InfixExpr a op l r) = do
  l' <- convert l
  r' <- convert r
  return $ Call (Ref $ show op) [l', r']
convert (CondExpr a cnd t f) = do
  cnd' <- convert cnd
  t'   <- convert t
  f'   <- convert f
  return (Call (Ref "cond") [cnd', Lambda [] (P [Return t']), Lambda [] (P [Return f'])])
convert (AssignExpr a op (LVar a' lv) e) = do
  e' <- convert e
  if op == OpAssign
    then modify (\f -> \cnt -> f [Assign lv e'] ++ cnt)
    else modify (\f -> \cnt -> f [Assign lv (Call (Ref $ show op) [Ref lv, e'])] ++ cnt)
  return $ Ref lv
convert (ListExpr a es) = do
  let go e@(AssignExpr _ _ _ _) = convert e
      go e                      = convert (AssignExpr a OpAssign (LVar a "@") e)
  last <$> mapM go es
convert (CallExpr a f xs) = do
  f'  <- convert f
  xs' <- mapM convert xs

  modify (\f -> \cnt -> f [Assign "@r" (Call f' xs')] ++ cnt)

  let getobj _    (DotRef _ lv _)      = getobj True lv
      getobj True (VarRef _ (Id _ lv)) = Just lv
      getobj False _                   = Nothing
      getobj _     _                   = Nothing

  whenJust (getobj False f) $ \n ->
    modify (\f -> \cnt -> f [Assign n (Call (Ref "fst") [Ref "@r"])] ++ cnt)

  modify (\f -> \cnt -> f [Assign "world" (Call (Ref "snd") [Ref "@r"])] ++ cnt)
  modify (\f -> \cnt -> f [Assign "@" (Call (Ref "trd") [Ref "@r"])] ++ cnt)

  return $ Ref "@"
convert (FuncExpr a (Just (Id a2 n)) xs ss) = undefined

convertS :: Statement a -> State ([S] -> [S]) S
convertS (BlockStmt a ss) = undefined
{-
convertS (EmptyStmt a)
convertS (ExprStmt a (Expression a))
convertS (IfStmt a (Expression a) (Statement a) (Statement a))
convertS (IfSingleStmt a (Expression a) (Statement a))
convertS (SwitchStmt a (Expression a) [CaseClause a])
convertS (WhileStmt a (Expression a) (Statement a))
convertS (DoWhileStmt a (Statement a) (Expression a))
convertS (BreakStmt a (Maybe (Id a)))
convertS (ContinueStmt a (Maybe (Id a)))
convertS (LabelledStmt a (Id a) (Statement a))
convertS (ForInStmt a (ForInInit a) (Expression a) (Statement a))
convertS (ForStmt a (ForInit a) (Maybe (Expression a)) (Maybe (Expression a)) (Statement a))
convertS (ForStmt a init test increment body, for (init; test, increment) body, spec 12.)
convertS (TryStmt a (Statement a) (Maybe (CatchClause a)) (Maybe (Statement a)))
convertS (ThrowStmt a (Expression a))
convertS (ReturnStmt a (Maybe (Expression a)))
convertS (WithStmt a (Expression a) (Statement a))
convertS (VarDeclStmt a [VarDecl a])
convertS (FunctionStmt a (Id a) [Id a] [Statement a])
-}

parseExpr str = case parse expression "" str of
  Right expr -> expr
  Left err   -> error $ show err

testConvert :: String -> P
testConvert e = P $ ss [Return (Call (Ref "merge") [e', Ref "world"])]
  where (e', ss) = runState (convert $ parseExpr e) id

texpr1 = parseExpr "a.exec('fn').push(b), noobj('arg'), a"

texpr2 = parseExpr "b = x, a = y, a = f(b), a = f(a), a = f(a), a = f(a), a"

texpr3 = parseExpr "a = y, b = x, a = f(b), a = f(a), a = f(a), a"
