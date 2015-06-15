{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
{-- | Parser for the lambda AST built of parsec. Converts to an intermediate format for antiexpressions -}
module Language.Lambda.Untyped.Parser where
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token 
import Language.Lambda.Untyped.Syntax
import Data.Functor.Identity
import Data.List
import Data.Data

type M = Identity

data MetaExpr s = MVar (MetaSym s)
              | MApp (MetaExpr s) (MetaExpr s)
              | MLam (MetaSym s) (MetaExpr s)
              | AntiExpr String
              | AntiVar  String
              deriving(Show, Eq, Data, Typeable)
              
data MetaSym s = S s
               | AntiSym String
              deriving(Show, Eq, Data, Typeable)

type Output s = MetaExpr s

type SymParser u s = ParsecT String u M s

top_expr :: SymParser u s -> ParsecT String u M (Output s)
top_expr sp = do 
    spaces
    e <- parse_expr sp
    spaces
    eof
    return e
    
parse_expr :: SymParser u s -> ParsecT String u M (Output s)
parse_expr sp = try (parse_aexpr sp)
          <|> try (parse_lambda sp)
          <|> try parse_anti_expr
 
parse_aexpr :: SymParser u s -> ParsecT String u M (Output s)
parse_aexpr sp =  try (parse_app sp)
              <|> try (parse_atom sp)
           
parse_anti_expr :: ParsecT String u M (Output s)
parse_anti_expr = do
    _ <- string "$"
    i <- (identifier haskell)
    return $ AntiExpr i

parse_lambda :: SymParser u s -> ParsecT String u M (Output s)
parse_lambda sp = do
    _ <- char '\\'
    spaces
    sym  <- (p_sym sp) <?> "lambda argument"
    _ <- char '.'
    spaces
    expr <- (parse_expr sp) <?> "lambda expression"
    return $ MLam sym expr

parse_app :: SymParser u s -> ParsecT String u M (Output s)
parse_app sp = do
    expr_0 <- (parse_atom sp) <?> "first apply argument"
    spaces
    as <-  sepBy1 (parse_atom sp) spaces <?> "other apply arguments"
    return $ foldl' MApp expr_0 as

parse_atom :: SymParser u s -> ParsecT String u M (Output s)
parse_atom sp =  try  (parens'  (parse_expr sp))
          <|> try (parse_var sp)
          <|> try parse_anti_expr
          
parse_var sp = try (parse_var' sp) <|> parse_anti_var 
          
parse_var' :: SymParser u s -> ParsecT String u M (Output s)
parse_var' sp = do
    spaces
    sym <- (p_sym sp) <?> "Var symbol"
    return $ MVar sym  
 
parse_anti_var = do 
    spaces 
    _ <- string "*"
    i <- (identifier haskell)
    return $ AntiVar i
    
p_sym :: SymParser u s -> ParsecT String u M (MetaSym s)
p_sym sp = try (S `fmap` sp) <|> try parse_anti_sym

parse_anti_sym :: ParsecT String u M (MetaSym s)
parse_anti_sym = do
    _ <- string "^"
    i <- (identifier haskell)
    return $ AntiSym i
    
parse_sym :: ParsecT String u M Sym
parse_sym = many1 (alphaNum <|> char '_') <?> "symbol"

parens' :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
parens' p = do 
    _ <- char '('
    e <- p
    _ <- char ')'
    return e

meta_to_expr :: MetaExpr s -> GExpr s
meta_to_expr (MVar (S x))     = Var x
meta_to_expr (MApp x y)   = App (meta_to_expr x) (meta_to_expr y)
meta_to_expr (MLam (S x) y)   = Lam x (meta_to_expr y)
meta_to_expr _ = error "meta_to_expr should not be used if the MetaExpr tree has AntiExpr"

to_meta :: GExpr s -> MetaExpr s
to_meta (Var x)   = MVar (S x)
to_meta (App x y) = MApp (to_meta x) (to_meta y)
to_meta (Lam x y) = MLam (S x) (to_meta y)






