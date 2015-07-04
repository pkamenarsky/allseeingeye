module IR.JS where

import           Control.Applicative                      ((<$>))
import           Control.Arrow
import           Control.Monad.State

import           Data.Generics.Str
import           Data.Generics.Uniplate.Data
import           Data.Data
import           Data.List
import qualified Data.Map                                 as M
import qualified Data.Set                                 as S
import           Data.Maybe

import           Language.ECMAScript3.Parser
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

import           IR

type VarIndex = [Int]

data Context = Context
  { unSS      :: [S] -> [S]
  , unWorld   :: M.Map String VarIndex
  , unIndex   :: VarIndex
  , unArgs    :: S.Set String
  , unAssign  :: S.Set Name
  , unLocals  :: S.Set String
  , unF       :: M.Map String E
  }

newDecl :: String -> State Context [Int]
newDecl decl = do
  st <- get
  put $ st { unWorld  = M.insert decl (unIndex st) (unWorld st)
           , unIndex  = init (unIndex st) ++ [last (unIndex st) + 1]
           , unLocals = S.insert decl (unLocals st)
           }
  return $ unIndex st

showDecl :: [Int] -> String
showDecl decl = "⟨" ++ intercalate " " (map show decl) ++ "⟩"

newBlock :: Context -> Context
newBlock st =
  st { unSS     = id
     , unIndex  = unIndex st ++ [1]
     , unArgs   = S.empty
     , unLocals = S.empty
     }

addAssign :: String -> State Context ()
addAssign v =  do
  st <- get
  unless (S.member v (unLocals st)) $ do
    v' <- resolveVar v
    put $ st { unAssign = S.insert v' (unAssign st) }

{-
addF :: String -> E -> State Context ()
addF v e = modify $ \st -> st { unF = M.insert v e (unF st) }
-}

exitBlock :: State Context ()
exitBlock = do
  st <- get
  put $ st { unIndex  = init (unIndex st) ++ [last (unIndex st) + 1] }

idContext :: Context
idContext = Context id M.empty [1] S.empty S.empty S.empty M.empty

-- function name, impure arguments (0 is this, -1 is world)
fns :: M.Map String [Int]
fns = M.fromList [ ( "push", [0] ) ]

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip (maybe (return ()))

pushBack st = modify (\ctx -> ctx { unSS = (\cnt -> unSS ctx [st] ++ cnt) })
pushFront st = modify (\ctx -> ctx { unSS = (\cnt -> unSS ctx cnt ++ [st]) })

getobj _    (DotRef _ lv _)      = getobj True lv
getobj True (VarRef _ (Id _ lv)) = Just lv
getobj False _                   = Nothing
getobj _     _                   = Nothing

resolveVar :: String -> State Context Name
resolveVar n = do
  st <- get
  case M.lookup n (unWorld st) of
    Just n' -> return $ if (S.member n (unLocals st))
      then Local $ showDecl n'
      else Bound $ showDecl n'
    Nothing -> return $ Global n

convertE :: Expression a -> State Context E
convertE (StringLit a lit) = return $ Const lit
convertE (RegexpLit a lit glb csi) = return $ Const lit
convertE (NumLit a lit) = return $ Const $ show lit
convertE (IntLit a lit) = return $ Const $ show lit
convertE (BoolLit a lit) = return $ Const $ show lit
convertE (NullLit a) = return $ Const "null"
{-
convertE (ArrayLit a es) = App (Lam "array" ((setA (Var "array") 0 (convertE $ head es))))
                              (App (Var "newArray") (Var "world"))

convertE (ObjectLit a [(Prop a, Expression a)])
convertE (ThisRef a)
-}
convertE (VarRef a (Id a' ref)) = Ref <$> resolveVar ref
convertE (DotRef _ e (Id _ ref)) = do
  e'   <- convertE e
  return $ Call (Ref $ Global "get") [Const ref, e']
convertE (BracketRef a e i) = do
  e' <- convertE e
  i' <- convertE i
  return $ Call (Ref $ Global "get_elem") [i', e']
convertE (NewExpr a f xs) = do
  f'  <- convertE f
  xs' <- mapM convertE xs
  return $ Call f' xs'
convertE (PrefixExpr a op e) = do
  e' <- convertE e
  return $ Call (Ref $ Global $ show op) [e']
convertE (UnaryAssignExpr a op (LVar a' lv)) = do
  let (opname, push) = op'
  st <- get
  lv' <- resolveVar lv
  push $ Assign lv' (Call (Ref $ Global opname) [Ref lv'])
  return $ Ref lv'
  where
    op' = getOp op
    getOp PrefixInc  = ("inc", pushBack )
    getOp PostfixInc = ("inc", pushFront)
    getOp PrefixDec  = ("dec", pushBack )
    getOp PostfixDec = ("dec", pushFront)
convertE (InfixExpr a op l r) = do
  l' <- convertE l
  r' <- convertE r
  return $ Call (Ref $ Global $ show op) [l', r']
convertE (CondExpr a cnd t f) = do
  cnd' <- convertE cnd
  t'   <- convertE t
  f'   <- convertE f
  return (Call (Ref $ Global "cond") [cnd', Lambda [] (P [Return t']), Lambda [] (P [Return f'])])
convertE (AssignExpr a op (LVar a' lv) e) = do
  st  <- get
  e'  <- convertE e
  lv' <- resolveVar lv
  addAssign lv
  pushBack $ Assign lv' e'
  return $ Ref lv'
convertE (AssignExpr a op (LDot a' lv fld) rv) = do
  lv' <- convertE lv
  rv' <- convertE rv
  st  <- get
  case getobj True lv of
    Just n -> do
      n' <- resolveVar n
      addAssign n
      pushBack $ Assign n' (Call (Ref $ Global "set") [Ref n', Const fld, rv', lv'])
      return $ (Call (Ref $ Global "get") [Ref n', Const fld, lv'])
      -- pushBack $ Assign n (Call (Ref "set") [lv', Const fld, rv'])
      -- return $ Ref n
    Nothing -> error "Dot expression without dotref?"
convertE (ListExpr a es) = do
  let go e@(AssignExpr _ _ _ _) = convertE e
      go e                      = convertE (AssignExpr a OpAssign (LVar a "@") e)
  last <$> mapM go es
{-
  o.f(x)
≈ ρ, σ, ω = f(o, x, ω)

  function f(x)
≈ function f(this, x, ω)
-}
convertE (CallExpr a f xs) = do
  f'  <- convertE f
  xs' <- mapM convertE xs
  st  <- get

  pushBack $ Assign (Local result) (Call f' (xs' ++ [Ref $ Local world]))

  whenJust (getobj False f) $ \n -> do
    n' <- resolveVar n
    pushBack $ Assign n' (Call (Ref $ Global worldFn) [Const object, Ref $ Local world])

  return (Ref $ Local result)
convertE (FuncExpr a n xs ss) = do
  st <- get
  let st' = execState (mapM newDecl (map (\(Id _ n) -> n) xs)) (newBlock st)
      (xs', st'') = flip runState st' $ do
        mapM_ convertS ss
        mapM (\(Id _ n) -> resolveVar n) xs
      ss' = unSS st'' $ []
      isR (Return _) = True
      isR _          = False
      lastR | null ss'  = False
            | otherwise = isR $ last ss'
      l   = Lambda (xs' ++ [Local world]) (P $ ss' ++ if lastR then [] else [Return (Ref $ Local world)])

  exitBlock

  case n of
    Just (Id _ n) -> do
      newDecl n
      n' <- resolveVar n
      pushBack $ Assign n' l
      return l
    _ ->
      return l

convertS :: Statement a -> State Context ()
convertS (BlockStmt a ss) = do
  st <- get
  let st' = execState (mapM_ convertS ss) (newBlock st)
      ss' = unSS st' []
      isR (Return _) = True
      isR _          = False
      lastR | null ss'  = False
            | otherwise = isR $ last ss'
  exitBlock
  -- pushBack $ Assign (Local result) (Call (Lambda [] (P (ss' ++ if lastR then [] else [Return (Call (Ref $ Global mergeFn) (map Ref $ (S.toList $ unAssign st')))]))) [])
  pushBack $ Assign (Local world) (Call (Lambda [Local world] (P (ss' ++ if lastR then [] else [Return (Ref $ Local world)]))) [Ref $ Local world])
convertS (EmptyStmt a) = return ()
convertS (ExprStmt a e@(AssignExpr _ _ _ _)) = convertE e >> return ()
convertS (ExprStmt a e) = convertE e >> return ()
{-
convertS (IfStmt a (Expression a) (Statement a) (Statement a))
convertS (IfSingleStmt a (Expression a) (Statement a))
convertS (SwitchStmt a (Expression a) [CaseClause a])
-}
convertS (WhileStmt a e s) = do
  -- e' <- convertE e
  -- let s' = execState (convertS s) id $ []
  -- pushBack $ Assign world $ Call (Ref worldFn) [Call (Ref "while") [Lambda [] (P [Return e']), Call (Lambda [world] (P $ s' ++ [Return (Call (Ref mergeFn) [Ref "@", Ref world])])) [Ref world]]]
  -- convertS s
  st <- get
  e' <- convertE e
  let s' = unSS (execState (convertS s) (newBlock st)) []
  exitBlock
  pushBack $ Assign (Local result) (Call (Ref $ Global "while") [e', Lambda [Local world] (P $ s' ++ [Return (Ref $ Local world)])])
{-
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
  st <- get
  e' <- convertE e
  pushBack $ Return e'
  -- pushBack $ Return (Call (Ref $ Global mergeFn) (e' : (map Ref (S.toList $ unAssign st))))
  -- pushBack $ Assign world (Call (Ref worldUpFn) [Const result, e', Ref world])
convertS (ReturnStmt a Nothing) = pushBack $ Return (Ref $ Local world)
{-
convertS (WithStmt a (Expression a) (Statement a))
-}
convertS (VarDeclStmt a decls) = do
  forM_ decls $ \(VarDecl a (Id a2 n) e) -> do
    decl <- newDecl n
    whenJust e $ \e' -> do
      e'' <- convertE e'
      n' <- resolveVar n
      pushBack $ Assign n' e''
{-
convertS (FunctionStmt a (Id a) [Id a] [Statement a])
-}

parseProgram str = case parse program "" str of
  Right expr -> expr
  Left err   -> error $ show err

parseExpr str = case parse expression "" str of
  Right expr -> expr
  Left err   -> error $ show err

testConvert :: String -> P
testConvert e = P $ unSS ss [Return $ Ref $ Local world]
  where (_, ss)       = runState (convertS $ BlockStmt a pr) idContext
        (Script a pr) = parseProgram e

texpr1 = parseExpr "a.exec('fn').push(b), noobj('arg'), a"

texpr2 = parseExpr "b = x, a = y, a = f(b), a = f(a), a = f(a), a = f(a), a"

texpr3 = parseExpr "a = y, b = x, a = f(b), a = f(a), a = f(a), a"
