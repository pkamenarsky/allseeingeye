module Language.Lambda.Common.Arbitrary (sym_arbitrary) where
import Test.QuickCheck
    
-- | Generates a string like "x_{n}" where n is positive integer
sym_arbitrary :: Gen String 
sym_arbitrary = do
    index <- suchThat (arbitrary :: Gen Int) (>0)
    return $ ("x_" ++ (show index))
