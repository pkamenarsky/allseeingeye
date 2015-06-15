{-- | Pretty printers for lambda expression -}
module Language.Lambda.SimplyTyped.Pretty where
import Language.Lambda.SimplyTyped.Syntax

-- | Pretty prints a Expr
ppr :: Show a => Expr String a String -> String
ppr (Var x)   = x
ppr (App x y) = "(" ++ ppr x ++ " " ++ ppr y ++ ")" 
ppr (Lam x t y) = "(\\" ++ x ++ ":" ++ ppr_type t ++ " ." ++ ppr y ++ ")"
ppr (Constant x) = x


ppr_type (Base x) = show x
ppr_type (Arrow x y) = "(" ++ ppr_type x ++ "->" ++ ppr_type y ++ ")"


