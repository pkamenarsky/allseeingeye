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

pushBack st = modify (\f -> \cnt -> f [st] ++ cnt)
pushFront st = modify (\f -> \cnt -> f cnt ++ [st])

convertE :: Expression a -> State ([S] -> [S]) E
convertE (StringLit a lit) = return $ Const lit
convertE (RegexpLit a lit glb csi) = return $ Const lit
convertE (NumLit a lit) = return $ Const $ show lit
convertE (IntLit a lit) = return $ Const $ show lit
convertE (BoolLit a lit) = return $ Const $ show lit
convertE (NullLit a) = return $ Ref "null"
{-
convertE (ArrayLit a es) = App (Lam "array" ((setA (Var "array") 0 (convertE $ head es))))
                              (App (Var "newArray") (Var "world"))

convertE (ObjectLit a [(Prop a, Expression a)])
convertE (ThisRef a)
-}
convertE (VarRef a (Id a' ref)) = return $ Ref ref
convertE (DotRef _ e (Id _ ref)) = do
  e' <- convertE e
  return $ Call (Ref "get") [e', Const ref]
convertE (BracketRef a e i) = do
  e' <- convertE e
  i' <- convertE i
  return $ Call (Ref "get_elem") [e', i']
convertE (NewExpr a f xs) = do
  f'  <- convertE f
  xs' <- mapM convertE xs
  return $ Call f' xs'
convertE (PrefixExpr a op e) = do
  e' <- convertE e
  return $ Call (Ref $ show op) [e']
convertE (UnaryAssignExpr a op (LVar a' lv))
  | (opname, True)  <- op' = do
      pushBack $ Assign lv (Call (Ref opname) [Ref lv])
      return $ Ref lv
  | (opname, False) <- op' = do
      pushFront $ Assign lv (Call (Ref opname) [Ref lv])
      return $ Ref lv
  where
    op' = getOp op
    getOp PrefixInc  = ("inc", True )
    getOp PostfixInc = ("inc", False)
    getOp PrefixDec  = ("dec", True )
    getOp PostfixDec = ("dec", False)
convertE (InfixExpr a op l r) = do
  l' <- convertE l
  r' <- convertE r
  return $ Call (Ref $ show op) [l', r']
convertE (CondExpr a cnd t f) = do
  cnd' <- convertE cnd
  t'   <- convertE t
  f'   <- convertE f
  return (Call (Ref "cond") [cnd', Lambda [] (P [Return t']), Lambda [] (P [Return f'])])
convertE (AssignExpr a op (LVar a' lv) e) = do
  e' <- convertE e
  if op == OpAssign
    then pushBack $ Assign lv e'
    else pushBack $ Assign lv (Call (Ref $ show op) [Ref lv, e'])
  return $ Ref lv
convertE (ListExpr a es) = do
  let go e@(AssignExpr _ _ _ _) = convertE e
      go e                      = convertE (AssignExpr a OpAssign (LVar a "@") e)
  last <$> mapM go es
convertE (CallExpr a f xs) = do
  f'  <- convertE f
  xs' <- mapM convertE xs

  pushBack $ Assign "@r" (Call f' xs')

  let getobj _    (DotRef _ lv _)      = getobj True lv
      getobj True (VarRef _ (Id _ lv)) = Just lv
      getobj False _                   = Nothing
      getobj _     _                   = Nothing

  whenJust (getobj False f) $ \n ->
    pushBack $ Assign n (Call (Ref "fst") [Ref "@r"])

  pushBack $ Assign "world" (Call (Ref "snd") [Ref "@r"])
  pushBack $ Assign "@" (Call (Ref "trd") [Ref "@r"])

  return $ Ref "@"
convertE (FuncExpr a n xs ss) = do
  let ss' = execState (mapM_ convertS ss) id $ []
      l   = Lambda (map (\(Id _ n) -> n) xs ++ ["world"]) (P ss')

  case n of
    Just (Id _ n) -> do
      pushBack $ Assign n l
      return $ Ref n
    _ ->
      return l

convertS :: Statement a -> State ([S] -> [S]) ()
convertS (BlockStmt a ss) = mapM_ convertS ss
convertS (EmptyStmt a) = return ()
convertS (ExprStmt a e@(AssignExpr _ _ _ _)) = convertE e >> return ()
convertS (ExprStmt a e) = do
  e <- convertE e
  pushBack $ Assign "@" e
{-
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
-}
convertS (ReturnStmt a (Just e)) = do
  e' <- convertE e
  pushBack $ Return (Call (Ref "merge") [e', Ref "world"])
convertS (ReturnStmt a Nothing) = pushBack $ Return (Ref "world")
{-
convertS (WithStmt a (Expression a) (Statement a))
convertS (VarDeclStmt a [VarDecl a])
convertS (FunctionStmt a (Id a) [Id a] [Statement a])
-}

parseProgram str = case parse program "" str of
  Right expr -> expr
  Left err   -> error $ show err

parseExpr str = case parse expression "" str of
  Right expr -> expr
  Left err   -> error $ show err

testConvertE :: String -> P
testConvertE e = P $ ss [Return (Call (Ref "merge") [e', Ref "world"])]
  where (e', ss) = runState (convertE $ parseExpr e) id

testConvert :: String -> P
testConvert e = P $ ss []
  where (_, ss)       = runState (convertS $ BlockStmt a pr) id
        (Script a pr) = parseProgram e

texpr1 = parseExpr "a.exec('fn').push(b), noobj('arg'), a"

texpr2 = parseExpr "b = x, a = y, a = f(b), a = f(a), a = f(a), a = f(a), a"

texpr3 = parseExpr "a = y, b = x, a = f(b), a = f(a), a = f(a), a"
