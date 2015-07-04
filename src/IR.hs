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
         | App a (L a) [L a]
         | Lam a [Name] (L a)
         | Let a Name (L a) (L a)
         | Letlocal a Name (L a) (L a)
         | Rho a [Name] (L a)
         -- | Merge a (M.Map String (L a))
         | Merge a [(String, L a)]
         | Hold a Name (L a)
         deriving (Eq, Ord, Data, Typeable)

data W = W { unW :: M.Map Name (L W) } deriving (Eq, Ord, Data, Typeable)

w = W M.empty

mergeFn = "⤚"
worldFn = "↖ω"
worldUpFn = "↪ω"
result = "ρ"
object = "σ"
world = "ω"

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

unTag :: L a -> a
unTag (Cnst w _)  = w
unTag (Var w _)   = w
unTag (Extrn w _) = w
unTag (App w _ _) = w
unTag (Lam w _ _) = w
unTag (Merge w _) = w

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

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

-}

replaceret :: ([L a] -> L a -> L a) -> L a -> L a
replaceret = go []
  where
    go ctx f (App w e@(Lam w2 ns lam) xs)
      | length ns == length xs = App w (Lam w2 ns (go (e:ctx) f lam)) (map (go [] f) xs)
      | otherwise              = error "replaceret: curried application"
    go ctx f e = f ctx e

boundmerge :: L a -> L a
boundmerge = replaceret f
  where
    f ctx e
      | null bounds = e
      | otherwise   = Merge (unTag e) $ ("ρ", e):map (\v -> (name v, Var (unTag e) v)) bounds
      where bounds = concatMap (\(Lam _ ns _) -> filter isBound ns) ctx
            isBound (Global _) = True
            isBound (Bound  _) = True
            isBound (Local  _) = False

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

normalize :: Show a => L a -> L a
-- normalize e | trace (show e) False = undefined
normalize (Cnst w c)   = Cnst w c
normalize (Var w n)    = Var w n
normalize (Hold w n e) = Hold w n $ normalize e
normalize (Merge w xs) = Merge w (map (second normalize) xs)
normalize (App w (Lam w2 ns f) ms)
  |  length ns == length ms
  && any isMerge (map normalize ms) = normalize {- $ trace_tag "app: " -} $ App w (Lam w2 ns' (normalize f)) xs'
  where
    (ns', xs') = unzip $ concatMap mkName $ zip ns $ map normalize ms

    mkName (n, Merge w3 xs) = map mkName' $ zip' [n] xs
      where mkName' (Just n , Just (s, e)) = (Local (name n), normalize e)
            mkName' (Nothing, Just (s, e)) = (Local s       , normalize e)
            mkName' (Just n , Nothing)     = error "mkName'"
    mkName (n, e)           = [(Local (name n), normalize e)]

    isMerge (Merge _ _) = True
    isMerge _           = False
{-
normalize (App w (Lam w2 n f) (Merge w3 xs))
  = normalize $ foldl (\a (n, e) -> App w2 (Lam w2 (Bound n) a) e) f (replace_1st (name n) $ M.toList xs)
  where replace_1st _ [] = []
        replace_1st n' ((n,x):xs) = (n',x):xs
normalize (Merge w xs) = -- (\x -> trace ("R: " ++ show res) x) $
  Merge w (M.insert "ρ" res $ M.unions $ map go $ reverse $ M.toList xs)
  where go (_, Merge w xs) = {- trace ("xs: " ++ show xs) $ -} M.delete "ρ" $ M.map normalize xs
        go (n, e)          = {- trace ("x: " ++ show e) $ -} M.singleton n (normalize e)

        res | Just (Merge w2 xs2) <- M.lookup "ρ" $ M.map normalize xs
            , Just r <- M.lookup "ρ" $ M.map normalize xs2 = r
            | Just r <- M.lookup "ρ" $ M.map normalize xs  = normalize r
            | otherwise = error "merge: no ρ"
normalize (App w (Merge w2 xs) (Merge w3 xs2))
  | Just f' <- M.lookup "ρ" xs
  , Just x' <- M.lookup "ρ" xs2 = Merge w2 (M.insert "ρ" (normalize $ App w f' x') $ M.union xs xs2)
  | otherwise                   = error "app: no ρ/ρ"
normalize (App w f (Merge w2 xs))
  | Just x' <- M.lookup "ρ" xs = Merge w2 (M.insert "ρ" (normalize $ App w f x') xs)
  | otherwise                  = error "app: no f/ρ"
normalize (App w (Merge w2 xs) x)
  | Just f' <- M.lookup "ρ" xs = Merge w2 (M.insert "ρ" (normalize $ App w f' x) xs)
  | otherwise                  = error "app: no ρ"
normalize (Lam w (Bound n) f) =  Lam w (Local n) (normalize (Merge w $ M.fromList [("ρ", normalize f), (n, Var w (Local n))]))
normalize (Lam w (Global n) f) =  Lam w (Local n) (normalize (Merge w $ M.fromList [("ρ", normalize f), (n, Var w (Local n))]))
-}
normalize (App w f x) = go (normalize f) (map normalize x)
  where go (Lam _ n e) x' | length n == length x' = normalize $ foldr (uncurry subst) e (zip n x')
                          | otherwise             = error "curried"
        go f' x'          = App w f' x'
normalize (Lam w n f) = Lam w n (normalize f)


simplify :: Show a => L a -> L a
simplify = normalize . boundmerge

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

rule6 = lam [local "x"] (varlocal "a") `app` [merge [("ρ", cnst "666"), ("a", cnst "777")]]
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

instance Show W where
#if 0
  show (W w) = "⟦" ++ intercalate ", " (map (\(k, v) -> k ++ " → " ++ show v) $ M.toList w) ++ "⟧"
#else
  show (W w) = "(W " ++ intercalate " " (map (\(k, v) -> show k ++ " → " ++ show v) $ M.toList w) ++ ")"
#endif

instance Show Name where
  show (Global n) = "G" ++ n
  show (Local n) =  "L" ++ n
  show (Bound n) =  "B" ++ n

instance Show a => Show (L a) where
  show (Cnst a c)   = c
  show (Var a n)    = show n
  show (Hold a n e) = "H{" ++ show n ++ ":" ++ show e ++ "}"
  show (Extrn a n)  = "⟨" ++ show n ++ "⟩"

#if 0
  show (App a f x)  = "(" ++ show f ++ " " ++ show x ++ show a ++ ")"
  show (Lam a n f)  = "(λ" ++ n ++ " → " ++ show f ++ show a ++ ")"
#else
  show (App a f x)                      = "(" ++ show f ++ " " ++ intercalate " " (map show x) ++ ")"
  show (Lam a n f)                      = "(λ" ++ intercalate " " (map show n) ++ " → " ++ show f ++ ")"
{-
  show (App a f@(Lam _ _ _) x@(App _ _ _))  = "(" ++ show f ++ ") (" ++ show x ++ ")"
  show (App a f x@(App _ _ _))              = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App a f@(Lam _ _ _) x)              = "(" ++ show f ++ ") " ++ show x ++ ""
  show (App a f x@(Lam _ _ _))              = "" ++ show f ++ " (" ++ show x ++ ")"
  show (App a f x)                          = "" ++ show f ++ " " ++ show x ++ ""
  show (Lam a n f)                          = "λ" ++ show n ++ " → " ++ show f ++ ""
-}
  show (Merge a xs)                         = "(⤚ " ++ intercalate " " (map (\(k, v) -> k ++ "{" ++ show v ++ "}") xs) ++ ")"
#endif
