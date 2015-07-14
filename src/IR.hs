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

data Name = Local  { name :: String }
          | Bound  { name :: String }
          | Global { name :: String }
          deriving (Eq, Ord, Data, Typeable)

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
         -- | App a (L a) [L a]
         | App a (L a) (L a)
         -- | Lam a [Name] (L a)
         | Lam a Name (L a)
         | Let a Name (L a) (L a)
         | Letlocal a Name (L a) (L a)
         | Rho a [Name] (L a)
         -- | Merge a (M.Map String (L a))
         | Merge a [(String, L a)]
         | Hold a Name (L a)
         deriving (Eq, Ord, Data, Typeable)

data W = W { unW :: M.Map Name (L W) } deriving (Eq, Ord, Data, Typeable)

w = ""

mergeFn = "⤚"
worldFn = "↖ω"
worldUpFn = "↪ω"
result = "ρ"
object = "σ"
world = "ω"

{-
-- multiarg
sToE :: E -> L W
sToE (Const c)     = Cnst w c
sToE (Ref r)       = Var w r
sToE (Call f xs)   = App w (sToE f) (map sToE xs)
sToE (Lambda ns p) = Lam w ns (sToP p)

sToP :: P -> L W
sToP (P [])              = error "no return"
sToP (P (Decl n e:ps))   = App w (Lam w [n] (sToP (P ps))) [sToE e]
sToP (P (Assign n e:ps)) = App w (Lam w [n] (sToP (P ps))) [sToE e]
sToP (P (Return e:_))    = sToE e
sToP (P (Ctrl e p:_))    = error "ctrl"
-}

sToE :: E -> L String
sToE (Const c)     = Cnst w c
sToE (Ref r)       = Var w r
sToE (Call f xs)   = foldl (App w) (sToE f) (map sToE xs)
sToE (Lambda ns p) = foldr (Lam w) (sToP p) ns

sToP :: P -> L String
sToP (P [])              = error "no return"
-- sToP (P (Decl n e:ps))   = App w (Lam w n (sToP (P ps))) (sToE e)
-- sToP (P (Assign n e:ps)) = App w (Lam w n (sToP (P ps))) (sToE e)
sToP (P (Decl n e:ps))   = Let w n (sToE e) (sToP (P ps))
sToP (P (Assign n e:ps)) = Let w n (sToE e) (sToP (P ps))
sToP (P (Return e:_))    = sToE e
sToP (P (Ctrl e p:_))    = error "ctrl"

unTag :: L a -> a
unTag (Cnst w _)    = w
unTag (Var w _)     = w
unTag (Extrn w _)   = w
unTag (App w _ _)   = w
unTag (Let w _ _ _) = w
unTag (Lam w _ _)   = w
unTag (Merge w _)   = w

tag :: a -> L a -> L a
tag w (Cnst _ c)    = Cnst w c
tag w (Var _ v)     = Var w v
tag w (Extrn _ v)   = Extrn w v
tag w (App _ f x)   = App w f x
tag w (Let _ k v e) = Let w k v e
tag w (Lam _ n f)   = Lam w n f
tag w (Merge _ ms)  = Merge w ms

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

{-
-- multiarg
subst :: Show a => Name -> L a -> L a -> L a
-- subst n e _ | trace ("s: " ++ show n ++ ", e: " ++ show e) False = undefined
subst _ _ e'@(Cnst w _) = e'
{-
subst n e e'@(Hold w n' _) | name n == name n'   = Hold w n' e
                           | otherwise           = e'
-}
subst n e e'@(Var w n') | name n == name n'   = e
                        | otherwise           = e'
subst n e e'@(App w f x)  = App w (subst n e f) (map (subst n e) x)
subst n e e'@(Lam w ns f) | name n `elem` map name ns = Lam w ns f -- don't substitute a bound var
                          | otherwise                 = Lam w ns (subst n e f)
subst n e e'@(Merge w xs) = Merge w (map (second $ subst n e) xs)
-}

-- subst_dbg :: Show a => Name -> L a -> L a -> L a
subst_dbg :: Name -> L String -> L String -> L String
subst_dbg n e e' = trace_n ("☀☀ [" ++ show n ++ "=" ++ show e ++ "]") e' $ subst n e e'

-- subst :: Name -> L a -> L a -> L a
subst :: Name -> L String -> L String -> L String
subst n e e' | name n == unTag e' = e
subst _ _ e'@(Cnst _ _) = e'
subst n e e'@(Hold w n' h) | name n == name n' = Hold w n' e
                           | otherwise         = Hold w n' (subst n e h)
-- subst n e e'@(Var w n') | name n == name n' = Hold w n' e
subst n e e'@(Var w n') | name n == name n' = tag (name n) e
                        | otherwise         = e'
subst n e e'@(App w f x) = App w (subst n e f) (subst n e x)
subst n e e'@(Let w n' k v) | name n == name n' = Let w n' (subst n e k) v
                            | otherwise         = Let w n' (subst n e k) (subst n e v)
subst n e e'@(Lam w n' f) | name n == name n'  = Lam w n' f
                          | otherwise          = Lam w n' (subst n e f)
-- subst n e e'@(Merge w (("ρ", r):ms))  = Merge w (("ρ", subst n e r):ms)
-- subst n e e'@(Merge w ms)  = Merge w (map f ms)
subst n e e'@(Merge w (("ρ", r):ms))  = Merge w (("ρ", subst n e r): map f ms)
  -- don't subst twice
  where f (k, Var w x) = (k, subst n e (Var w x))
        f e            = e

{-
ρ = f …

(λρ → …) (⤚ x y z) ≈ (λx → (λy → (λz → …) z) y) x

add5(array) {
  array.push_back(5);
  log();
  g = f(array);
  return 666;
}

a = add5(x);

    v

add5(array, ω) {
  array = array.push_back(5);
  ω = log(ω);
  g = f(array);
  return ⤚(666, ω, array, g);
}

ρ<a, ω> = add5(x, ω); // expands to a, ω, x, g = add5(x, ω);

--

(λρ<a, ω> → …) f

if f unbound | (λρ → (λa → (λω → …) ↖2(ρ)) ↖1(ρ)) f
otherwise    | (λρ<a, ω> →  …) (⤚ x y U V)
             ≈ (λa → (λω → (λU → (λV → …) V) U) y) x

---

lambda ≈ let x = a in letlocal y = b in … in ⤚(x, …) | x not local

let x = ⤚(Ta, Tb, Tω) in ... ≈ let a = Ta in let b = Tb in let ω = Tω in …

---

a = f(x)      | ρ<a, ω> = f(x, ↖ω), use ↖a instead of a
n.push(x)     | ρ<n, ω> = push(n, x, ↖ω)
r = c.push(x) | ρ<r, c, ω> = push(c, x, ↖ω)

let[C] x = a in … ≈ ((λx → …) a)[x:C]
(f a)[x, y, z, …] ≈ ⤚(f a) x y z …

---

(λX → …) a ≈ … [X → a<X>]

   (λX → (λY → + X Y) b) a
 ≈ (λX → ⤚(+ X b<Y>) b<Y>) a
 ≈ ⤚a<X> (⤚(+ X b<Y>) b<Y>)
 ≈ ⤚(+ X b<Y>) b<Y> a<X>

   (λX → (λY → 5) b) a
 ≈ (λX → ⤚5 b<Y>) a
 ≈ ⤚5 b<Y> a<X>

---

(λx → ⤚ (λu → …) y)
(λx → ⤚ (λu → ⤚ u v) y)
(λx → ⤚ x y) (⤚ u v)

=== ⤚ RULES ===
(⤚ x y) u                 ≈ ⤚ (x u) y
u (⤚ x y)                 ≈ ⤚ (u x) y
⤚ (⤚ x y) (⤚ u v)         ≈ ⤚ x y u v
===============

(⤚ x y) (⤚ u v)           ≈ ⤚ (x (⤚ u v)) y ≈ ⤚ (⤚ (x u) v) y ≈ ⤚ (x u) v y

-}

{-
replaceret :: ([L a] -> L a -> L a) -> L a -> L a
replaceret = go []
  where
    go ctx f (App w e@(Lam w2 ns lam) xs)
      | length ns == length xs = App w (Lam w2 ns (go (e:ctx) f lam)) (map (go [] f) xs)
      | otherwise              = error "replaceret: curried application"
    go ctx f e@(Lam w ns lam)  = f ctx (Lam w ns (go [] f lam))
    go ctx f e@(App w app xs)  = f ctx (App w (go [] f app) (map (go [] f) xs))
    go ctx f e                 = f ctx e

boundmerge :: Show a => L a -> L a
boundmerge = replaceret f
  where
    f ctx e
      -- | trace ("\nm: " ++ show e) False = undefined
      | null bounds = e
      | otherwise   = Merge (unTag e) $ ("ρ", e):map (\v -> (name v, Var (unTag e) v)) bounds
      where bounds = concatMap (\(Lam _ ns _) -> filter isBound ns) ctx
            isBound (Global _) = True
            isBound (Bound  _) = True
            isBound (Local  _) = False
-}

boundmerge :: Show a => L a -> L a
boundmerge (Cnst w c) = Cnst w c
boundmerge (Var w n) = Var w n
boundmerge (Let w (Bound k) v e)
  = Let w (Local k) (boundmerge v) (Merge w [("ρ", boundmerge e), (k, Var w (Local k))])
boundmerge (Let w k v e) = Let w k (boundmerge v) (boundmerge e)
boundmerge (Lam w (Bound n) x)
  = Lam w (Local n) (Merge w [("ρ", boundmerge x), (n, Var w (Local n))])
boundmerge (Lam w n x) = Lam w n (boundmerge x)
boundmerge (App w f x) = App w (boundmerge f) (boundmerge x)

{-
  (λf → ⤚ ρ<f 5> c d) (λx → ⤚ ρ<x> a b)
≈ ⤚ ρ<⤚ ρ<x> a b> c d
≈ ⤚ ρ<x> a b c d

but this is automatically done by the destruct rule:

  (λρ<a, b> → …) (⤚ x y z)
≈ (λa → (λb → (λz → …) z) y) x

-}

zip' :: [a] -> [b] -> [(Maybe a, Maybe b)]
zip' [] []         = []
zip' (x:xs) (y:ys) = (Just x, Just y) :zip' xs ys
zip' (x:xs) []     = (Just x, Nothing):zip' xs []
zip' [] (y:ys)     = (Nothing, Just y):zip' [] ys

trace_tag str x = trace (str ++ show x) x

deleteElem :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteElem k = filter ((/= k) . fst)

unionElems :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
unionElems m1 m2 = M.toList (M.fromList m1 `M.union` M.fromList m2)

-- trace_n :: Show a => String -> L a -> L a -> L a
trace_n :: String -> L String -> L String -> L String
trace_n rule before after = trace (" ★ " ++ rule ++ " ★ " ++ show before ++ " ▶ " ++ show after) after
-- trace_n rule before after = after

{-# NOINLINE newName #-}
newName :: () -> String
newName () = "_r" ++ show (unsafePerformIO $ randomRIO (0, 10000) :: Int)

-- normalize :: Show a => L a -> L a
normalize :: L String -> L String
-- normalize e | trace (show e) False = undefined
normalize (Cnst w c)   = Cnst w c
normalize (Var w n)    = Var w n
normalize (Hold w n e) = Hold w n $ normalize e
normalize e@(Let w n (Merge w2 (("ρ", r):ms)) f)
    = normalize $ trace_n "let merge" e
                $ foldl (\cnt (n, v) -> Let w (Local n) v cnt)
                        -- substitue r for ρ because of the rholam rule
                        -- (Let w (Local nname) r (subst n (Var w (Local nname)) f))
                        (Let w n r f)
                        ms
                where nname = newName ()
{-
normalize e@(App w (Lam w2 n f) (Merge w3 (("ρ", r):ms)))
    = normalize $ trace_n "lam merge" e
                $ foldl (\cnt (n, v) -> App w (Lam w2 (Local n) cnt) v)
                        -- substitue r for ρ because of the rholam rule
                        (App w (Lam w2 (Local "_r") (subst (Local "ρ") (Var w2 (Local "_r")) f)) r)
                        ms
-}
normalize e@(App w (Lam w2 n f) (Merge w3 (("ρ", r):ms)))
    = normalize $ trace_n "lam merge" e
                $ foldl (\cnt (n, v) -> Let w (Local n) v cnt)
                        (Let w n r f)
                        ms
-- ⤚ (⤚ x y) (⤚ u v)         ≈ ⤚ x y u v
normalize e@(Merge w (("ρ", Merge w2 (("ρ", r):ms)):ms2))
  = normalize $ trace_n "nested merge" e $ Merge w (("ρ", r):unionElems ms ms2)
-- (⤚ x y) u                 ≈ ⤚ (x u) y
normalize e@(App w (Merge w2 (("ρ", r):ms)) x)
  = normalize $ trace_n "merge app" e $ Merge w2 (("ρ", App w r x):ms)
-- u (⤚ x y)                 ≈ ⤚ (u x) y
normalize e@(App w f (Merge w2 (("ρ", r):ms)))
  = normalize $ trace_n "app merge" e $ Merge w2 (("ρ", App w f r):ms)
normalize (Merge w xs) = Merge w (map (second normalize) xs)
{-
-- multiarg
normalize (App w f x) = go (normalize f) (map normalize x)
  where go (Lam _ n e) x' | length n == length x' = normalize $ foldr (uncurry subst) e (zip n x')
                          | otherwise             = error "curried"
        go f' x'          = App w f' x'
-}
-- normalize e'@(Let w k v e) | name k == "ρ" = normalize $ Let w (Local "ρ") (normalize v) e
normalize e'@(Let w k v e) = normalize $ trace_n "let" e' $ subst_dbg k (normalize v) e
-- normalize e'@(App w (Lam w2 n f) x) | name n == "ρ" = trace_n "rholam" e' $ App w (Lam w2 n f) (normalize x)
normalize e'@(App w f x) = go (normalize f) (normalize x)
  where go e''@(Lam _ n e) x' = normalize $ trace_n ("subst[" ++ show n ++ "=" ++ show x' ++ "]") e'' $ subst_dbg n x' e
        go f' x'          = trace_n "app" e' $ App w f' x'
normalize e@(Lam w n f) = trace_n "lam" e $ Lam w n (normalize f)

simplify :: Show a => L a -> L a
simplify = undefined
-- simplify = normalize . boundmerge

u = undefined
app = App u
lam = Lam u
var = Var u
global = Global
local = Local
bound = Bound
varglobal = Var u . Global
varlocal = Var u . Local
varbound = Var u . Bound
cnst = Cnst u
merge = Merge u

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

instance Show Name where
  show (Global n) = "G" ++ n
  show (Local n) =  "L" ++ n
  show (Bound n) =  "B" ++ n

showtag tag | null tag  = ""
            | otherwise = "<" ++ show tag ++ ">"

-- instance Show a => Show (L a) where
instance Show (L String) where
  show (Cnst a c)   = c ++ showtag a
  show (Var a n)    = show n
  show (Hold a n e) = "H{" ++ show n ++ ":" ++ show e ++ "}"
  show (Let a k v e)= "let " ++ show k ++ " = " ++ show v ++ " in " ++ show e
  -- show (Hold a n e) = show e
  show (Extrn a n)  = "⟨" ++ show n ++ "⟩"

#if 0
  show (App a f x)  = "(" ++ show f ++ " " ++ show x ++ show a ++ ")"
  show (Lam a n f)  = "(λ" ++ n ++ " → " ++ show f ++ show a ++ ")"
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
  show (Merge a xs)                         = "(⤚ " ++ intercalate " " (map (\(k, v) -> k ++ "{" ++ show v ++ "}") xs) ++ ")"
#endif
