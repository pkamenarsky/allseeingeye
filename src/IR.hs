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

{-
import           Language.Lambda.Untyped.Parser
import           Language.Lambda.Untyped.Syntax
import           Language.Lambda.Untyped.Quote
-}

import Debug.Trace

data Name = Local  String
          | Bound  String
          | Global String deriving (Eq, Ord, Data, Typeable)

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
         | Let a Name (L a) (L a)
         | Letlocal a Name (L a) (L a)
         | Rho a [Name] (L a)
         | Merge a [(String, L a)]
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
sToE (Call f xs)   = foldl (App w) (sToE f) (map sToE xs)
sToE (Lambda ns p) = foldr (Lam w) (sToP p) ns

sToP :: P -> L W
sToP (P [])              = error "no return"
sToP (P (Decl n e:ps))   = App w (Lam w n (sToP (P ps))) (sToE e)
sToP (P (Assign n e:ps)) = App w (Lam w n (sToP (P ps))) (sToE e)
sToP (P (Return e:_))    = sToE e
sToP (P (Ctrl e p:_))    = error "ctrl"

{-
unTag :: L a -> a
unTag (Cnst w _)  = w
unTag (Var w _)   = w
unTag (Extrn w _) = w
unTag (App w _ _) = w
unTag (Lam w _ _) = w

removeresobj :: W -> W
removeresobj = W . M.delete "ρ" . M.delete "σ" . unW

tag :: L W -> L W
tag (Cnst w c)  = Cnst w c
tag (Var w c) = Var w c
tag (Extrn w c) = Extrn w c
tag (App w f@(App _ (App _ (Var _ "↪ω") (Cnst _ k)) v) x)
  = App (W $ unW w `M.union` (M.insert k v $ unW $ unTag $ tag x)) (tag f) (tag x)

tag (App _ f@(Var _ _) x)   = App (removeresobj $ unTag $ tag x) f (tag x) -- f is unbound
tag (App _ f@(Cnst _ _) x)  = App (removeresobj $ unTag $ tag x) f (tag x) -- f is unbound
tag (App _ f@(Extrn _ _) x) = App (removeresobj $ unTag $ tag x) f (tag x) -- f is unbound

tag (App _ f x) = App (W dw) (tag f) (tag x)
  where dw = (unW $ unTag $ tag x) `M.difference` (unW $ unTag $ tag f)
tag (Lam w n f) = Lam w n (tag f)

filltag :: L W -> L W
filltag (Cnst w c)  = Cnst w c
filltag (Var w c) = Var w c
filltag (Extrn w c) = Extrn w c
filltag (App w1 (App w2 (Var w3 "↖ω") (Cnst w4 k)) w)
  | Just v <- M.lookup k (unW $ unTag w) = filltag v
  | otherwise = App w1 (App w2 (Var w3 "↖ω") (Cnst w4 k)) (filltag w)
filltag (App w f x) = App w (filltag f) (filltag x)
filltag (Lam w n f) = Lam w n (filltag f)

{-
f x y (↪ω ... ω) ≈ f x y ω    | if f unbound
↖ω k (↪ω k' v' ω) ≈ ↖ω k ω    | if k ≠ k'
-}

removetag :: L W -> L W
removetag (Cnst w c)  = Cnst w c
removetag (Var w c) = Var w c
removetag (Extrn w c) = Extrn w c
removetag (App w f@(App w2 (App w3 (Var w4 "↪ω") (Cnst w5 k)) v) x)
  | k /= "ρ" && k /= "σ" = removetag x
  | otherwise = App w (App w2 (App w3 (Var w4 "↪ω") (Cnst w5 k)) (removetag v)) (removetag x)
removetag (App w f x) = App w (removetag f) (removetag x)
removetag (Lam w n f) = Lam w n (removetag f)
-}

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

-- simplify :: L W -> L W
-- simplify = fixpoint (normalize . tag)

{-
rule1 = App w (App w (App w (Var w "↪ω") (Var w "k")) (Cnst w "v")) (Var w "ω")
rule2 = App w (Var w "f") ((App w (App w (App w (Var w "↪ω") (Var w "k'")) (Cnst w "v'"))) rule1)
-}

-- ω            = []
-- ↪ω k v ω     = [k: v]
-- ↪ω k v e[t]  = t ∪ [k: v]
-- f a[t]       = t             | if f unbound
-- ↖ω k e[t]    = v             | t ≈ [k: v, …]

-- ↖ω k (f x y (↪ω k v ω))        ≈ v | if k not captured by f
-- ↖ω σ (↪ω k v (↪ω k' v' (f ω))) ≈ ↖ω σ (f ω) | k, k' ≠ σ

{-
rewriteW :: L -> L
rewriteW (Cnst c)  = Cnst c
rewriteW (Var "ω") = W M.empty
rewriteW (Var c)   = Var c
rewriteW (Extrn c) = Extrn c
rewriteW (Var "↪ω" `App` Var k `App` v `App` W w) = W $ M.insert k v w
{-
rewriteW (W w1 `App` W w2) = W $ M.unions [w1, w2]
rewriteW (Var "↪ω" `App` Var k `App` w) = merge $ rewriteW w
  where merge (W w1 `App` W w2) = W $ M.unions [w1, w2]
        merge (v `App` W w)     = W $ M.insert k v w
        merge w                 = W $ M.singleton k w
-}
rewriteW (f `App` x) = rewriteW f `App` rewriteW x
rewriteW (n `Lam` f) = n `Lam` rewriteW f
-- rewriteW (W w)       = W $ M.map rewriteW w
rewriteW (W w)       = W w
-}

subst :: Name -> L a -> L a -> L a
subst _ _ e'@(Cnst _ _) = e'
subst n e e'@(Var _ n') | n == n'   = e
                        | otherwise = e'
subst n e e'@(App w f x) = App w (subst n e f) (subst n e x)
-- subst n e e'@(Let k v x) = Let k (subst n e v) x
subst n e e'@(Lam w n' f)  = Lam w n' (subst n e f)
subst n e e'@(Merge w xs)  = Merge w (map (second (subst n e)) xs)

{-
↪ω k v
↖ω k ω
f … ω         | only keep ↪ω k v … if k bound in f

log(x)
log(y)

ω = log(x, ω)
ω = log(y, ω)

(λω → (λω → ω) (log y ω)) (log x ω)

↖ω k (f … (↪ω k v)) ≈ v?

---

(λω -> … ) ω ≈ …

---

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

normalize :: L String -> L String
normalize (Cnst w c)  = (Cnst w c)
normalize (Var w n)   = (Var w n)
{-
normalize (App w (App w2 (Cnst w3 "⤚") x) y) = go x y
  where go (App w4 (Cnst w5 "⤚") x') (Var w6 n) = undefined
-}
normalize (Merge w xs) = Merge w (map (second normalize) xs)
normalize (App w f x) = go (normalize f) (normalize x)
  where go (Lam _ n e) x' = normalize $ subst n x' e
        go f' x'        = App w f' x'
normalize (Lam w (Bound n) f) =  Lam w (Local n) (normalize (Merge w [("ρ", normalize f), (n, Var w (Local n))]))
normalize (Lam w (Global n) f) =  Lam w (Local n) (normalize (Merge w [("ρ", normalize f), (n, Var w (Local n))]))
normalize (Lam w n f) = Lam w n (normalize f)
normalize e = e

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

rule1 = cnst "⤚" `app` varlocal "x" `app` varlocal "y"
rule2 = lam (local"f") (lam (bound "x") (varlocal "f" `app` varlocal "x" `app` varlocal "y"))
rule3 = rule2 `app` (lam (local "a") (lam (local "b") (varglobal "*" `app` varlocal "a" `app` varlocal "b"))) `app` cnst "5"

{-
-- 1. (λa → …) b          ≈ let a = b in …
rewriteLam :: L -> L
rewriteLam (Cnst c)            = (Cnst c)
rewriteLam (Var n)             = (Var n)
rewriteLam (Extrn n)           = (Extrn n)
rewriteLam (Lam [] f `App` []) = rewriteLam f
rewriteLam (Lam ns f `App` xs) = Let (last ns) (rewriteLam $ last xs)
                                     (rewriteLam (Lam (init ns) f `App` (init xs)))
rewriteLam (Lam ns f)          = Lam ns (rewriteLam f)
rewriteLam (f `App` xs)        = rewriteLam f `App` map rewriteLam xs
rewriteLam (Let k v cnt)       = Let k (rewriteLam v) (rewriteLam cnt)

{-
2.  f a                 ≈ let x = a in f x
    ↖ω k (↪ω k v ω)     ≈ let ω = ↪ω k v ω in ↖ω k ω
2a. ↖ω k (f (↪ω k v ω)) ≈ let ω = ↪ω k v ω in ↖ω k ω | if k not captured by f
-}
rewriteLet :: L -> L
rewriteLet = go (const Nothing)
  where go :: (Name -> Maybe L) -> L -> L
        go ctx (Cnst c)   = Cnst c
        -- go ctx (Var "ω")  = undefined
        go ctx (Var n)    = Var n
        go ctx (Extrn n)  = Extrn n
        go ctx e@(Var "↖ω" `App` [Var k, Var "ω"])
          | Just v <- ctx k = v
          | otherwise       = e
        go ctx (Let "ω" (Var "↪ω" `App` [Var k, v, Var "ω"]) cnt)
          = go (\k' -> if k == k' then Just (go ctx v) else ctx k') (subst "ω" (Var "↪ω" `App` [Var k, go ctx v, Var "ω"]) cnt)
        -- go ctx (Let "ω" v (Var "ω")) = Var "W"
        -- go ctx (Let k v cnt) = ([k] `Lam` go ctx cnt) `App` [go ctx v]
        go ctx (Let k v cnt) = Let k (go ctx v) (go ctx cnt)
        go ctx (f `App` xs) = go ctx f `App` map (go ctx) xs
        go ctx (ns `Lam` f) = ns `Lam` go ctx f

rewriteInOut :: L -> L
rewriteInOut (Cnst c)  = (Cnst c)
rewriteInOut (Var n)   = (Var n)
rewriteInOut (Extrn n) = (Extrn n)
rewriteInOut (Var "↖ω" `App` [k, x])
  | Just x' <- go x = x'
  | otherwise       = Var "↖ω" `App` [rewriteInOut k, rewriteInOut x]
  where go (Var "↪ω" `App` [uk, uv, ux])
           | k == uk   = Just uv
           | otherwise = go ux
        -- go (App f x) = go f <|> go x
        go _ = Nothing
rewriteInOut (App f x) = App (rewriteInOut f) (map rewriteInOut x)
rewriteInOut (Lam n f) = Lam n (rewriteInOut f)
rewriteInOut (W w) = W $ M.map rewriteInOut w

rewriteL :: L -> L
rewriteL (Cnst c)  = (Cnst c)
rewriteL (Var n)   = (Var n)
rewriteL (Extrn n) = (Extrn n)
-- ↖ω ρ (f … ω)      ≈ f … (WRONG!)
-- ↖ω ρ (f … (↪ω …)) ≈ f … (WRONG!)
#if 0
rewriteL e@(Var "↖ω" `App` Var "ρ" `App` r@(f `App` x))
  | Just r' <- go r = (\x -> trace ("BEFORE: " ++ showL e ++ "\nAFTER : " ++ showL r' ++ "\n") x) r'
  | otherwise       = Var "↖ω" `App` Var "ρ" `App` rewriteL r
  where go (f `App` (Var "ω"))          = Just f
        go (f `App` (Var "↪ω" `App` _)) = Just f
        go (f `App` x)                  = App f <$> go x
        go _                            = Nothing
-- ↖ω s (… (↪ω s v ω)) ≈ v
--
-- r = f(a) → (r, a, ω) = f(a, ω) → ω = f(a, ω)
--   return r → ↪ω ρ r (↪ω σ obj ω)
--
-- let ω = ↪ω k v ω in let ω = ↪ω k' v' ω in f (↖ω k ω) ≈ f v
#endif
#if 0
rewriteL e@(Var "↖ω" `App` Var k `App` W w)
  | Just v <- M.lookup k w = v
  | otherwise              = e
#endif
#if 0
rewriteL (Var "↖ω" `App` k `App` x)
  | Just x' <- go x = x'
  | otherwise       = Var worldFn `App` rewriteL k `App` rewriteL x
  where go (Var "↪ω" `App` uk `App` uv `App` ux)
           | k == uk   = Just uv
           | otherwise = go ux
        -- go (App f x) = go f <|> go x
        go _ = Nothing
#endif
-- IMPORTANT: (↪ω k v (↪ω k' v' ω)) ≈ (↪ω k' v' (↪ω k v ω)) !!!
-- must try all possible permutations for next rule; better use W (map)
-- f x y (↪ω k v ω) ≈ ↪ω k v (f x y ω) | k not captured by f
--
-- let ω = ↪ω k v ω in f x y ω
rewriteL (f `App` []) = rewriteL f `App` []
rewriteL e@(f `App` xs)
  -- | trace ("f: " ++ showL f ++ "\n") False = undefined
  | Just f' <- go (last xs) = f'
  | otherwise = rewriteL f `App` map rewriteL xs
  where
    go (Var "↪ω" `App` [k, v, w])
       | captured f k = -- (\x -> trace ("before: " ++ showL e ++ "\nafter: " ++ fromMaybe "" (showL <$> x)) x) $
         Just $ Var "↪ω" `App` [rewriteL k, rewriteL v, rewriteL f `App` (map rewriteL (init xs) ++ [rewriteL w])]
       | otherwise    = Nothing
    go _ = Nothing

    captured (Var "↪ω") _ = False
    captured (Var "↖ω") _ = False
    captured (Var "push_back") _ = False
    -- captured (Var "↪ω") _ = False
    -- captured e _ = trace ("cap: " ++ showL e ++ "\n") True
    -- TODO: only if k is not captured by f!
    captured f k                  = True
{-
rewriteL (Var "get" `App` obj `App` field)
  | (value:_) <- [ value | (Var "set" `App` _ `App` field' `App` value) <- universeL obj, field == field' ] = rewriteL value
  | otherwise = (Var "get" `App` rewriteL obj `App` rewriteL field)
-}
rewriteL (Lam n f) = Lam n (rewriteL f)
rewriteL (W w) = W $ M.map rewriteL w
#if 0
rewriteL (Var "get" `App` k `App` x)
  | Just x' <- go x = x'
  | otherwise       = Var "get" `App` rewriteL k `App` rewriteL x
  where go (Var "set" `App` uk `App` uv `App` ux)
           | k == uk   = Just uv
           | otherwise = go ux
        go _ = Nothing
#endif
-- ↖ω σ (↪ω k v (↪ω k' v' (f ω))) ≈ ↖ω σ (f ω) | k, k' ≠ σ
--
-- let ω = ↪ω k' v' (f ω) in let ω = ↪ω k v ω in ↖ω σ ω ≈ ↖ω σ (f ω)

showL (Cnst c)   = c
showL (Var n)    = n
showL (Extrn n)  = "⟨" ++ n ++ "⟩"

--showL (App f x)  = "(" ++ showL f ++ " " ++ showL x ++ ")"
--showL (Lam n f)  = "(λ" ++ n ++ " → " ++ showL f ++ ")"

showL (App f [])                     = showL f
showL (App f x)                      = "(" ++ showL f ++ " " ++ intercalate " " (map showL x) ++ ")"
showL (Lam n f)                      = "λ" ++ intercalate " " n ++ " → " ++ showL f ++ ""
showL (W w)                          = "⟦" ++ intercalate " : " (map (\(k, v) -> k ++ " → " ++ showL v) $ M.toList w) ++ "⟧"
showL (Let n v e)                    = "(let " ++ n ++ " = " ++ showL v ++ " in " ++ showL e ++ ")"

-- Tests
rule1 = Var "f" `App` [Var "↪ω" `App` [Var "k", Var "v", Var "ω"]]
rule1a = Var "f" `App` [Var "g" `App` [Cnst "const", Var "↪ω" `App` [Var "k", Var "v", Var "ω"]]]
rule1b = Var "↪ω" `App` [Var "k", Var "v", Var "f" `App` [Var "x", Var "y", Var "↪ω" `App` [Var "k'", Var "v'", Var "↪ω" `App` [Var "k''", Var "v''", Var "ω"]]]]
rule1c = Var "↪ω" `App` [Var "k", Var "v", Var "↪ω" `App` [Var "k'", Var "v'", Var "f" `App` [Var "x", Var "y", Var "↪ω" `App` [Var "k''", Var "v''", Var "ω"]]]]
{-

rule2 = Var "↖ω" `App` Var "a" `App` (Var "↪ω" `App` Var "b" `App` Var "0" `App` (Var "↪ω" `App` Var "a" `App` Var "1" `App` Var "ω"))
-}

simplify :: L -> L
simplify = fixpoint (rewriteL . rewriteInOut . normalize)
-- simplify = fixpoint (rewriteL . rewriteW . normalize)

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
