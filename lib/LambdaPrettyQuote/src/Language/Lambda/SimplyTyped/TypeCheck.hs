module Language.Lambda.SimplyTyped.TypeCheck (type_check) where 
import Language.Lambda.SimplyTyped.Syntax
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Tuple.Select
import Control.Arrow
    
type M = Identity
type TypeError = String

type TypeCheckEnv a s c o = ErrorT TypeError (ReaderT ((c -> Type a), Env a s) M) o

type_check :: (Show s, Eq s, Eq a, Show a) => (c -> Type a) -> Expr s a c -> Either TypeError (Type a)
type_check const_to_type expr = runIdentity $ 
    runReaderT (runErrorT (type_check' expr)) (const_to_type, [])

------------------------------------------------------------------------------------------------------

type Env a s = [(s, Type a)]

extend :: s -> Type a -> Env a s -> Env a s
extend sym typ xs = (sym, typ):xs

find_var :: (Show s, Eq s) => s -> TypeCheckEnv a s c (Type a)
find_var s = unmaybe ("could not find variable named " ++ show s) =<< (lift $ asks (lookup s . sel2))
        
type_check' :: (Show s, Eq s, Eq a, Show a) => Expr s a c -> TypeCheckEnv a s c (Type a)
type_check' (Var sym)   = find_var sym
type_check' (App function argument) = do
    function_type <- type_check'  function
    --split the function type into its input and output
    function_input_type  <- input_type  function_type 
    function_output_type <- output_type function_type

    --argument must match function input
    argument_type <- type_check' argument
    when (function_input_type /= argument_type) $ 
        throwError $ ("Type Error: expected " ++ show function_input_type ++ " but got" 
            ++ show argument_type)

    return function_output_type
type_check' (Lam sym input_typ expr) = do
   local (second (extend sym input_typ)) $ do
        output_typ <- type_check' expr
        return $ Arrow input_typ output_typ
type_check' (Constant c) = asks (($ c) . sel1)
    
    
unmaybe :: String -> Maybe o -> TypeCheckEnv a s c o
unmaybe error_string Nothing  = throwError error_string
unmaybe error_string (Just x) = return x

input_type (Arrow x _) = return x
input_type _           = throwError "Not a function type"
    
output_type (Arrow _ y) = return y
output_type _           = throwError "Not a function type"
    
    
    
    
    
    
    
    
    
    
    
    
    
    