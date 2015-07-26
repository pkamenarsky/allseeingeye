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

data Name = Bound [Int] | Extern String deriving (Eq, Ord, Data, Typeable)

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
         deriving (Eq, Ord, Data, Typeable)

w = ""

mergeFn = Extern "⤚"
worldFn = Extern "↖ω"
worldUpFn = Extern "↪ω"
result = Extern "ρ"
object = Extern "σ"
world = Extern "ω"

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

trace_n :: Show a => String -> L a -> L a -> L a
{-
trace_n rule before after = unsafePerformIO $ do
  putStrLn rule
  return after
-}
-- trace_n rule before after = trace (" ★ " ++ rule ++ " ★ " ++ show before ++ " ▶ " ++ show after) after
trace_n rule before after = after

-- trace_n2 rule before after = trace (" ★ " ++ rule ++ " ★ " ++ show before ++ " ▶ " ++ show after) after
trace_n2 rule before after = after

{-# NOINLINE newName #-}
newName :: () -> String
newName () = "_r" ++ show (unsafePerformIO $ randomRIO (0, 10000) :: Int)

subst_dbg :: Show a => Name -> L a -> L a -> L a
subst_dbg n e e' = trace_n ("☀☀ [" ++ show n ++ "=" ++ show e ++ "]") e' $ subst n e e'

subst :: Show a => Name -> L a -> L a -> L a
subst _ _ e'@(Cnst _ _) = e'
subst n e e'@(Var w n') | n == n'   = e
                        | otherwise = e'
subst n e e'@(App w f x) = App w (subst n e f) (subst n e x)
subst n e e'@(Lam w n' f) | n == n'   = trace ("SAME: " ++ show n) e'
                          | otherwise = Lam w n' (subst n e f)

normalize :: Show a => L a -> L a
normalize (Cnst w c)   = Cnst w c
normalize (Var w n)    = Var w n
normalize e'@(App w f x) = go (normalize f) (normalize x)
  where
    go e''@(Lam _ n e) x' = normalize $ trace_n ("subst[" ++ show n ++ "=" ++ show x' ++ "]") e'' $ subst_dbg n x' e
    -- TODO: complete drilling edge cases
    go f@(App w3 (Var w4 (Extern "↖ω")) (Var w5 k))
       x@(App _ (App _ (App _ (Var _ (Extern "↪ω"))
                              (Var _ k'))
                       v)
                cnt)
      | k == k' = trace_n2 ("drill [" ++ show k ++ " = " ++ show v ++ "]") x $ v
      | otherwise = go f cnt

    go f' x'              = trace_n "app" e' $ App w f' x'
normalize e@(Lam w n f) = trace_n "lam" e $ Lam w n (normalize f)

simplify :: Show a => L a -> L a
simplify = normalize

u = undefined
app = App u
lam = Lam u
var = Var u
cnst = Cnst u
extern = Var . Extern

subtree :: L a -> L a -> Maybe (L a)
subtree (Cnst w c1) (Cnst _ c2)
  | c1 == c2  = Just $ Cnst w c1
  | otherwise = Nothing
subtree (Var w c1) (Var _ c2)
  | c1 == c2  = Just $ Var w c1
  | otherwise = Nothing
subtree (App w f1 xs1) (App _ f2 xs2) = App w <$> subtree f1 f2 <*> subtree xs1 xs2
subtree (Lam w n1 f1) (Lam _ n2 f2)
  | n1 == n2  = Lam w n1 <$> subtree f1 f2
  | otherwise = Nothing
subtree _ _ = Nothing

tlength :: Data a => L a -> Int
tlength = length . universe

lmtree :: Data a => L a -> L a -> Maybe (L a)
lmtree l@(App _ f1 xs1) r@(App _ f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree l f2
                            , lmtree l xs2
                            , lmtree r f1
                            , lmtree r xs1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l@(Lam _ n1 f1) r@(Lam _ n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree l f2
                            , lmtree r f1
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree l r = subtree l r

lmtree1 :: Data a => L a -> L a -> Maybe (L a)
lmtree1 l@(App _ f1 xs1) r@(App _ f2 xs2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree1 l f2
                            , lmtree1 l xs2
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree1 l@(Lam _ n1 f1) r@(Lam _ n2 f2)
 | trees@(x:_) <- catMaybes [ subtree l r
                            , lmtree1 l f2
                            ]
   = Just $ maximumBy (compare `on` tlength) trees
 | otherwise = Nothing
lmtree1 l r = subtree l r

instance Show Name where
  show (Bound xs) = "<" ++ intercalate ", " (map show xs) ++ ">"
  show (Extern n) = n

instance Show a => Show (L a) where
  show (Cnst a c)   = c
  show (Var a n)    = show n
--   show (Extrn a n)  = "⟨" ++ show n ++ "⟩"

--  show ((App _ (App _ (Var _ (Extern "↖ω")) k) (Var _ (Extern "ω")))) = "⟨" ++ show k ++ "⟩"
#if 1
  show (App a f x)  = "(" ++ show f ++ " " ++ show x ++ ")"
  show (Lam a n f)  = "(λ" ++ show n ++ " → " ++ show f ++ ")"
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
