{- | This module provides the code of QuickCheck instances, but doesn't declare the instances
   Of the whole orphan deal, oh well. 
 
    Anyway, to use you will need to copy the following code somewhere
> Instance Arbitrary Expr where
>     arbitrary = expr_arb  
>     shrink    = gexpr_shrink
 There is also a gexpr_arb that takes in a generator for the symbol type
-}
module Language.Lambda.Untyped.Arbitrary (
    expr_arb,
    gexpr_shrink, 
    gexpr_arb,
    gexpr_arb'
) where
import Test.QuickCheck
import Control.Applicative ((<*>), (<$>))
import Language.Lambda.Untyped.Syntax
import Language.Lambda.Common.Arbitrary

-- | An arbitrary function for Expr. See the example above.
expr_arb :: Gen Expr
expr_arb = gexpr_arb sym_arbitrary
    
-- | Shrink function for an GExpr. See the example at the top of the module    
gexpr_shrink :: GExpr a -> [GExpr a]
gexpr_shrink x@(Var _) = [x]
gexpr_shrink (App x y) = [x, y]
gexpr_shrink (Lam _ y) = [y]
   
-- | Helper function for creating generators for GExpr. Takes in a generator for the symbol type
gexpr_arb :: Gen a -> Gen (GExpr a) 
gexpr_arb sym_gen = sized $ \x -> gexpr_arb' x sym_gen

-- | Helper function for creating generators for GExpr. Takes in a generator for the symbol type and the
-- "depth" of the expression tree
gexpr_arb' :: Int -> Gen s -> Gen (GExpr s)
gexpr_arb' 0 s_arb = Var <$> s_arb
gexpr_arb' n s_arb = do
    option <- choose(0, 2) :: Gen Int
    case option of
        0 -> Var <$> s_arb
        1 -> App <$> gexpr_arb' (n - 1) s_arb <*> gexpr_arb' (n - 1) s_arb
        2 -> Lam <$> s_arb                    <*> gexpr_arb' (n - 1) s_arb
        _ -> error "choose messed up!"
        
