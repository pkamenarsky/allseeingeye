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

setA :: L -> Int -> L -> L
setA arr i e = (App (App (App (Var "setA") arr) (Cnst $ show i)) e)

-- function name, impure arguments (0 is this, -1 is world)
fns :: M.Map String [Int]
fns = M.fromList [ ( "push", [0] ) ]

prefixOp :: M.Map UnaryAssignOp (String, Bool)
prefixOp = M.fromList [ (PrefixInc , ("inc", True ))
                      , (PostfixInc, ("inc", False))
                      , (PrefixDec , ("dec", True ))
                      , (PostfixDec, ("dec", False))
                      ]

convert :: Expression a -> L -> L
convert (StringLit a lit) cnt = Cnst lit
convert (RegexpLit a lit glb csi) cnt = Cnst lit
convert (NumLit a lit) cnt = Cnst $ show lit
convert (IntLit a lit) cnt = Cnst $ show lit
convert (BoolLit a lit) cnt = Cnst $ show lit
convert (NullLit a) cnt = Var "null"
{-
convert (ArrayLit a es) = App (Lam "array" ((setA (Var "array") 0 (convert $ head es))))
                              (App (Var "newArray") (Var "world"))

convert (ObjectLit a [(Prop a, Expression a)])
convert (ThisRef a)
-}
convert (VarRef a (Id a' ref)) cnt = Var ref
convert (DotRef _ e (Id _ ref)) cnt = App (App (Extrn "get") (convert e undefined)) (Cnst ref)
{-
convert (BracketRef a (Expression a) {- container -} (Expression a) {- key -})
convert (NewExpr a (Expression a) {- constructor -} [Expression a])
convert (PrefixExpr a PrefixOp (Expression a))
-}
convert (UnaryAssignExpr a op (LVar a' lv)) cnt =
  (App (Lam lv cnt) (App (Extrn $ fst $ M.findWithDefault ("op", True) op prefixOp) (Var lv)))
{-
convert (InfixExpr a InfixOp (Expression a) (Expression a))
convert (CondExpr a (Expression a) (Expression a) (Expression a))
-}
convert (AssignExpr a op (LVar a' lv) e) cnt =
  let cnt' = App (Lam lv cnt) (convert e cnt') in cnt'
convert (ListExpr a es) cnt | null es   = cnt
                            | otherwise = foldr convert cnt (filter (not . isRef) (init es) ++ [last es])
  -- we need to filter all useless refs inside the list, like "y" in
  -- x++, y, z
  where isRef (VarRef _ _)       = True
        isRef (DotRef _ _ _)     = True
        isRef (BracketRef _ _ _) = True
        isRef (ThisRef _)        = True
        isRef _                  = False
convert (CallExpr a f xs) cnt
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
convert (FuncExpr a n xs ss) cnt = undefined

unnestAssigns :: Data a => Expression a -> ([Expression a], Expression a, [Expression a])
unnestAssigns e = case e of
  e1@(AssignExpr a op (LVar a1 lv) (VarRef a2 ref)) ->
    ([e1], VarRef a1 (Id a1 lv), [])
  e1@(AssignExpr a op (LVar a1 lv) e2) -> let (pre, e3, post) = unnestAssigns e2 in
    (pre ++ [AssignExpr a op (LVar a1 lv) e3], VarRef a1 (Id a1 lv), post)
  e1@(UnaryAssignExpr a op (LVar a1 lv))
    | Just (_, True) <- M.lookup op prefixOp -> ([e1], VarRef a1 (Id a1 lv), [])
    | otherwise                              -> ([], VarRef a1 (Id a1 lv), [e1])
  e | null pre && null post -> ([], e, [])
    | otherwise             -> (pre, unstr $ unch ch2, post)
    where (str, unstr) = uniplate e
          (ch,  unch)  = strStructure str
          unnested     = map unnestAssigns ch
          pre          = concat [ x | (x, _, _) <- unnested ]
          post         = concat [ x | (_, _, x) <- unnested ]
          ch2          = [ x | (_, x, _) <- unnested ]

unnest :: Data a => Expression a -> Expression a
unnest e | null pre && null post = e
         | otherwise = ListExpr (getAnnotation e) (pre ++ post ++ [e'])
  where (pre, e', post) = unnestAssigns e

type Vars a = State Int a

u = undefined

getVar :: Vars Name
getVar = do
  x <- get
  modify (+1)
  return $ "x" ++ show x

tuple :: [Name] -> Expression a -> Vars [Expression a]
tuple vars e = do
  tpl <- getVar
  return $ [AssignExpr u OpAssign (LVar u tpl) e]
        ++ map (\(i, var) -> AssignExpr u OpAssign (LVar u var)
            (CallExpr u (VarRef u (Id u ("get" ++ show i))) [(VarRef u (Id u tpl))])) (zip [1..] vars)

unnestCallExpr :: Data a => Expression a -> Vars [Expression a]
unnestCallExpr e@(CallExpr a (VarRef _ _) xs)    = return [e]
unnestCallExpr e@(CallExpr a (DotRef _ e2 _) xs) = do
  var <- getVar
  e2' <- unnestCallExpr e2
  return $ [AssignExpr u OpAssign (LVar u var) e] ++ e2'

parseExpr str = case parse expression "" str of
  Right expr -> expr
  Left err   -> error $ show err

--testExpr = case parse expression "" "x = [y = f(z = ++a)]" of
-- testExpr = case parse expression "" "r = rand(y, ++z, world), world = fst(r), x = snd(r), tuple(world, x)" of
texpr1 = parseExpr "a.exec('fn').push(b), noobj('arg'), a"
tc1 = convert (unnest texpr1) (Lam "xxx" (Var "xxx"))

texpr2 = parseExpr "a = y, b = x, a = f(a), a = f(a), a"
tc2 = convert (unnest texpr2) (Lam "xxx" (Var "xxx"))

texpr3 = parseExpr "a = y, b = x, a = f(a), a"
tc3 = convert (unnest texpr3) (Lam "xxx" (Var "xxx"))
