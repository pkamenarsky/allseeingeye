{-- | Pretty printers for lambda expression -}
module Language.Lambda.Untyped.Pretty where
import Language.Lambda.Untyped.Syntax

-- | Pretty prints a Expr
ppr :: Expr -> String
ppr (Var x)   = x
ppr (App x y) = "(" ++ ppr x ++ " " ++ ppr y ++ ")" 
ppr (Lam x y) = "(\\" ++ x ++ "." ++ ppr y ++ ")"

-- | Pretty prints a GExpr 
g_ppr :: (Show a) => GExpr a -> String
g_ppr (Var x)   = show x
g_ppr (App x y) = "(" ++ g_ppr x ++ " " ++ g_ppr y ++ ")" 
g_ppr (Lam x y) = "(\\" ++ show x ++ "." ++ g_ppr y ++ ")"