module Language.Lambda.SimplyTyped.Arbitrary (
    Env,
    gen_type,
    gen_expr,
    gen_with_env,
    shrink_expr
) where
import Test.QuickCheck
import Control.Applicative ((<*>), (<$>))
import Data.List
import Language.Lambda.SimplyTyped.Syntax
import Control.Monad.RWS.Strict
import Control.Applicative
import Test.QuickCheck.Gen
import Data.Tuple.Select
import Language.Lambda.Common.Arbitrary
import Data.Maybe
import Debug.Trace.Helpers
import Debug.Trace

type Env a s = [(s, Type a)]

null_const_gen = arbitrary :: Gen (Maybe ())

gen_type :: (Eq a, Eq s) => Gen a -> Gen s -> Gen (Type a)
gen_type x y = gen_with_env arb_type x y $ const null_const_gen

gen_expr :: (Eq a, Eq s) => Gen a -> Gen s -> (Type a -> Gen (Maybe c)) -> Gen (Expr s a c)
gen_expr = gen_with_env arb_expr

shrink_expr :: Expr s a c -> [Expr s a c]
shrink_expr x@(Constant _) = []
shrink_expr x@(Var _)      = []
shrink_expr (App x y)      = []
shrink_expr (Lam s t e)    = []

gen_with_env :: (Eq a, Eq s) => EnvGen a s c b -> Gen a -> Gen s -> (Type a -> Gen (Maybe c)) -> Gen b
gen_with_env gen x y z = sized $ \i -> gen_with_env' gen (min i 5) x y z
    
gen_with_env' :: (Eq a, Eq s) => EnvGen a s c b -> Int -> Gen a -> Gen s -> (Type a -> Gen (Maybe c)) -> Gen b
gen_with_env' gen size a_gen s_gen c_gen = do
    (result, _) <- evalRWST gen (a_gen, s_gen, size, c_gen, []) ()
    return result

type EnvGen a s c = RWST (Gen a, Gen s, Int, Type a -> Gen (Maybe c), (Env a s)) () () Gen

arb_type :: (Eq a, Eq s) => EnvGen a s c (Type a)
arb_type = arb_type' =<< get_size 

arb_type' :: (Eq a, Eq s) => Int -> EnvGen a s c (Type a)
arb_type' 0    = Base <$> arb_base
arb_type' size = do
    option <- lift (choose (0, 1 :: Int))
    case option of
        0 -> Base  <$> arb_base
        1 -> Arrow <$> arb_type' (size - 1) <*> arb_type' (size - 1)

arb_expr :: (Eq a, Eq s) => EnvGen a s c (Expr s a c)
arb_expr = do
    i <- get_size 
    input  <- arb_type
    output <- arb_type
    arb_lam i (Arrow input output)

--This is good, but it is not useful for 
arb_var typ = do
    lookuped_value <- lookup_var_by_type typ
    case lookuped_value of
        (Just x) -> return $ Var $ fst x
        _ -> error "bad symbol name"
 
var_type_exists :: (Eq a, Eq s) => Type a -> EnvGen a s c (Bool)
var_type_exists typ = isJust <$> lookup_var_by_type typ
    
lookup_var_by_type :: (Eq a, Eq s) => Type a -> EnvGen a s c (Maybe (s, Type a))
lookup_var_by_type typ = do
    vars <- gets_env (filter ((typ==) . snd))
    if length vars > 0 
        then do v <- lift $ elements vars
                return $ Just v 
        else return Nothing
 
-- the right has to be it
-- and the left has to be type -> whatever it was told to be  
arb_app i typ = do
     output_type <- arb_type
     let f = Arrow output_type typ
     arb_app_typ i f output_type

arb_app_typ i input_type output_type = do
    App <$> arb_expr' input_type (i - 1) <*> arb_expr' output_type (i - 1)


fourth f (x, y, z, w, u) = (x, y, z, w, u)

arb_lam :: (Eq a, Eq s) => Int -> Type a -> EnvGen a s c (Expr s a c) 
arb_lam 0 x = terminal_lambda x
arb_lam i (Arrow input output) = do
    sym <- uniq_sym
    local (fourth (extend sym input)) $ do 
        Lam sym input <$> (arb_expr' output (i - 1))

arb_expr' :: (Eq a, Eq s) => Type a -> Int -> EnvGen a s c (Expr s a c)
arb_expr' typ i = do
    option <-lift $ choose (0, 10 :: Int)
    if option == 0
        then attemp_constant_expr typ i  
        else non_constant_expr typ i
        
app_or_lam typ 0 = terminal_lambda typ 
app_or_lam typ@(Arrow _ _) i = do
    option <- lift arbitrary
    if option
        then arb_app i typ
        else arb_lam i typ
app_or_lam typ i = arb_app i typ
        
terminal_lambda :: (Eq a, Eq s) => Type a -> EnvGen a s c (Expr s a c) 
terminal_lambda typ@(Base _) = do 
    c <- arb_constant typ
    return $ Constant $ fromJust c
terminal_lambda (Arrow input output) = do
    sym <- uniq_sym
    Lam sym input <$> terminal_lambda output

     
non_constant_expr :: (Eq a, Eq s) => Type a -> Int -> EnvGen a s c (Expr s a c)
non_constant_expr typ@(Arrow _ _) 0 = arb_lam 0 typ
non_constant_expr typ@(Arrow _ _) i = do
        option <- lift $ choose (0, 2 :: Int)
        case option of 
            0 -> do can_make_var <- var_type_exists typ
                    if can_make_var
                        then arb_var typ
                        else app_or_lam typ i
            1 -> arb_app i typ
            2 -> arb_lam i typ
non_constant_expr typ@(Base _) i = do
        c <- arb_constant typ
        return $ Constant $ fromJust c

attemp_constant_expr :: (Eq a, Eq s) => Type a -> Int -> EnvGen a s c (Expr s a c)
attemp_constant_expr typ i = do
    constant <- arb_constant typ
    case constant of
        Just x  -> return $ Constant x
        Nothing -> non_constant_expr typ i
            
extend :: (Eq a, Eq s) => s -> Type a -> Env a s -> Env a s
extend s t xs = (s, t):xs

get_env :: (Eq a, Eq s) => EnvGen a s c (Env a s)
get_env = asks sel5

gets_env :: (Eq a, Eq s) => (Env a s -> d) ->  EnvGen a s c d
gets_env f = asks (f . sel5)
    
arb_s :: (Eq a, Eq s) => EnvGen a s c s
arb_s = lift =<< asks sel2
    
arb_base :: (Eq a, Eq s) => EnvGen a s c a
arb_base = lift =<< asks sel1

get_size :: (Eq a, Eq s) => EnvGen a s c Int
get_size = asks sel3

arb_constant :: (Eq a, Eq s) => (Type a) -> EnvGen a s c (Maybe c)
arb_constant x = lift =<< asks (($ x) . sel4)

uniq_sym :: (Eq a, Eq s) => EnvGen a s c s
uniq_sym = do
    s_gen <- asks sel2
    env <- get_env
    lift $ suchThat s_gen ( `notElem` (map fst env))


    
    
    
    
    
    
    
    
    

