{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, QuasiQuotes, TypeSynonymInstances #-}

module IR where

import           Control.Arrow                (second)
import           Control.Applicative          ((<$>), (<*>), (<|>), pure)
import           Control.Monad
import           Control.Monad.State

import           Data.Data
import           Data.Function
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Typeable

import           Data.Generics.Uniplate.Data

import           Graph

import System.IO.Unsafe
import System.Random

import Debug.Trace

type Name = String

data CtrlType

data E = Const String
       | Ref Name
       | Call E [E]
       | Lambda [Name] P
       deriving (Eq, Ord, Data, Typeable)

data S = Decl Name E
       | Assign Name E
       | Destruct [Name] E
       | Return E
       | Ctrl E P
       deriving (Eq, Ord, Data, Typeable)

data P = P [S] deriving (Eq, Ord, Data, Typeable)

data L a = Cnst a String
         | Var a Name
         | Extrn a Name
         | App a (L a) (L a)
         | Lam a Name (L a)
         | W a (M.Map Name (L a))
         deriving (Eq, Ord, Data, Typeable)

w = ""

mergeFn = "⤚"
worldFn = "↖ω"
worldUpFn = "↪ω"
result = "ρ"
object = "σ"
world = "ω"

sToE :: E -> L String
sToE (Const c)     = Cnst w c
sToE (Ref r)       = Var w r
sToE (Call f xs)   = foldl (App w) (sToE f) (map sToE xs)
sToE (Lambda ns p) = foldr (Lam w) (sToP p) ns

sToP :: P -> L String
sToP (P [])              = error "no return"
sToP (P (Decl n e:ps))   = App w (Lam w n (sToP (P ps))) (sToE e)
sToP (P (Assign n e:ps)) = App w (Lam w n (sToP (P ps))) (sToE e)
sToP (P (Return e:_))    = sToE e
sToP (P (Ctrl e p:_))    = error "ctrl"

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

trace_tag str x = trace (str ++ show x) x

deleteElem :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteElem k = filter ((/= k) . fst)

unionElems :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
unionElems m1 m2 = M.toList (M.fromList m1 `M.union` M.fromList m2)

-- trace_n :: Show a => String -> L a -> L a -> L a
trace_n :: String -> L String -> L String -> L String
-- trace_n rule before after = trace (" ★ " ++ rule ++ " ★ " ++ show before ++ " ▶ " ++ show after) after
trace_n rule before after = after

{-# NOINLINE newName #-}
newName :: () -> String
newName () = "_r" ++ show (unsafePerformIO $ randomRIO (0, 10000) :: Int)

-- subst_dbg :: Show a => Name -> L a -> L a -> L a
subst_dbg :: Name -> L String -> L String -> L String
subst_dbg n e e' = trace_n ("☀☀ [" ++ show n ++ "=" ++ show e ++ "]") e' $ subst n e e'

-- subst :: Name -> L a -> L a -> L a
subst :: Name -> L String -> L String -> L String
subst _ _ e'@(Cnst _ _) = e'
subst n e e'@(Var w n') | n == n'   = e
                        | otherwise = e'
subst n e e'@(App w f x) = App w (subst n e f) (subst n e x)
subst n e e'@(Lam w n' f) | n == n'   = Lam w n' f
                          | otherwise = Lam w n' (subst n e f)
subst n e e'@(W w m) | Just v <- M.lookup "ρ" m = subst n e v
                     | otherwise = e'

-- normalize :: Show a => L a -> L a
normalize :: L String -> L String
-- normalize e | trace (show e) False = undefined
normalize (Cnst w c)   = Cnst w c
normalize (Var w n)    = Var w n
normalize e'@(App w f x) = go (normalize f) (normalize x)
  where
    go e''@(Lam _ n e) x' = normalize $ trace_n ("subst[" ++ show n ++ "=" ++ show x' ++ "]") e'' $ subst_dbg n x e
    go (App w2 (App w3 (Var w4 "↪ω") (Cnst w5 k)) v) (Var w6 "ω")
                          = W w (M.singleton k (normalize v))
    go (App w2 (App w3 (Var w4 "↪ω") (Cnst w5 k)) v) (W w6 m)
                          = W w (M.insert k (normalize v) m)
    go (App w3 (Var w4 "↖ω") (Cnst w5 k)) (W w6 m)
      | Just v <- M.lookup k m = normalize v
      | otherwise              = App w (App w3 (Var w4 "↖ω") (Cnst w5 k)) (W w6 m)
    go f' x'              = trace_n "app" e' $ App w f' x'
normalize e@(Lam w n f) = trace_n "lam" e $ Lam w n (normalize f)
normalize e@(W w m)     = W w (M.map normalize m)

simplify :: Show a => L a -> L a
simplify = undefined

u = undefined
app = App u
lam = Lam u
var = Var u
cnst = Cnst u

-- rule6 = lam [local "x"] (varlocal "a") `app` [merge [("ρ", cnst "666"), ("a", cnst "777")]]
{-
rule1 = cnst "⤚" `app` varlocal "x" `app` varlocal "y"
rule2 = lam (local"f") (lam (bound "x") (varlocal "f" `app` varlocal "x" `app` varlocal "y"))
rule3 = rule2 `app` (lam (bound "a") (lam (local "b") (varglobal "*" `app` varlocal "a" `app` varlocal "b"))) `app` cnst "5"
rule4 = lam (bound "a") (lam (bound "b") (varglobal "+" `app` varlocal "a" `app` varlocal "b") `app` cnst "6") `app` cnst "5"
rule5 = lam (local "a") (varglobal "*" `app` varlocal "a") `app` merge (M.fromList [("x", varglobal "x"), ("y", varglobal "y")])
-}

{-
subtree :: L -> L -> Maybe L
#if 0
subtree (Var "↪ω" `App` Var "ρ" `App` x1) (Var "↪ω" `App` Var "ρ" `App` x2)
  = (Var "↪ω" `App` Var "ρ" `App`) <$> subtree x1 x2
subtree (Var "↪ω" `App` _) (Var "↪ω" `App` _) = Just $ Var world
#endif
subtree (Var "↪ω" `App` _) (Var "ω") = Just $ Var world
subtree (Var "ω") (Var "↪ω" `App` _) = Just $ Var world
subtree (Cnst c1) (Cnst c2)
  | c1 == c2  = Just $ Cnst c1
  | otherwise = Nothing
subtree (Var c1) (Var c2)
  | c1 == c2  = Just $ Var c1
  | otherwise = Nothing
subtree (App f1 xs1) (App f2 xs2) = App <$> subtree f1 f2 <*> subtree xs1 xs2
subtree (Lam n1 f1) (Lam n2 f2)
  | n1 == n2  = Lam n1 <$> subtree f1 f2
  | otherwise = Nothing
subtree _ _ = Nothing

tlength = length . universe

lmtree :: L -> L -> Maybe L
lmtree l@(App f1 xs1) r@(App f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree l f2
                            , lmtree l xs2
                            , lmtree r f1
                            , lmtree r xs1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l@(Lam n1 f1) r@(Lam n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree l f2
                            , lmtree r f1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l r = subtree l r

lmtree1 :: L -> L -> Maybe L
lmtree1 l@(App f1 xs1) r@(App f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree1 l f2
                            , lmtree1 l xs2
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree1 l@(Lam n1 f1) r@(Lam n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree1 l f2
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree1 l r = subtree l r
-}

{-
instance Show W where
#if 0
  show (W w) = "⟦" ++ intercalate ", " (map (\(k, v) -> k ++ " → " ++ show v) $ M.toList w) ++ "⟧"
#else
  show (W w) = "(W " ++ intercalate " " (map (\(k, v) -> show k ++ " → " ++ show v) $ M.toList w) ++ ")"
#endif
-}

-- instance Show a => Show (L a) where
instance Show (L String) where
  show (Cnst a c)   = c
  show (Var a n)    = n
  show (Extrn a n)  = "⟨" ++ n ++ "⟩"

#if 1
  show (App a f x)  = "(" ++ show f ++ " " ++ show x ++ ")"
  show (Lam a n f)  = "(λ" ++ n ++ " → " ++ show f ++ ")"
#else
{-
  show (App a f x)                      = "(" ++ show f ++ " " ++ intercalate " " (map show x) ++ ")"
  show (Lam a n f)                      = "(λ" ++ intercalate " " (map show n) ++ " → " ++ show f ++ ")"
-}
  show (App a f@(Lam _ _ _) x@(App _ _ _))  = "(" ++ show f ++ ") (" ++ show x ++ ")"
  show (App a f x@(App _ _ _))              = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App a f@(Lam _ _ _) x)              = "(" ++ show f ++ ") " ++ show x ++ ""
  show (App a f x@(Lam _ _ _))              = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App a f x)                          = "" ++ show f ++ " " ++ show x ++ ""
  show (Lam a n f)                          = "λ" ++ show n ++ " → " ++ show f ++ ""
#endif
  show (W a m)                              = "{" ++ intercalate "," (map (\(k, v) -> k ++ "=" ++ show v) $ M.toList m) ++ "}"
