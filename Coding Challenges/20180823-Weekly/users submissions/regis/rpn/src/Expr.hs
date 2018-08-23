module Expr
( run
, expr
, Expr
) where

import Control.Applicative ((<|>))
import Text.Parser.Combinators (try)
import Text.Parser.Token (TokenParsing, parens, symbolic, natural)
import Text.ParserCombinators.ReadP (readP_to_S)

-- The set of arithmetic expressions on integers
data Expr = I Integer      -- { ... -2, -1, 0, 1, 2, ... }
          | Sum Expr Expr  -- e1 + e2
          | Sub Expr Expr  -- e1 - e2
          | Mul Expr Expr  -- e1 * e2
          | Div Expr Expr  -- e1 / e2
          | Exp Expr Expr  -- e1 ^ e2
          | Paren Expr     -- (e)

-- RPN output
instance Show Expr where
  showsPrec d (I i) = shows i
  showsPrec d (Sum e1 e2) = showsPrec d e1 . showString " " . showsPrec d e2 . showString " +"
  showsPrec d (Sub e1 e2) = showsPrec d e1 . showString " " . showsPrec d e2 . showString " -"
  showsPrec d (Mul e1 e2) = showsPrec d e1 . showString " " . showsPrec d e2 . showString " *"
  showsPrec d (Div e1 e2) = showsPrec d e1 . showString " " . showsPrec d e2 . showString " /"
  showsPrec d (Exp e1 e2) = showsPrec d e1 . showString " " . showsPrec d e2 . showString " ^"
  showsPrec d (Paren e)   = showsPrec d e

-- | Expression parser. 
-- expr   ::= expr '+' term | expr '-' term | term
-- term   ::= term '*' factor | term '/' factor | factor
-- factor ::= '(' expr ')' | natural
-- 
-- Directly translated, left-recursive grammars lead to infinite
-- recursion, so we must transform them into right-recursive ones.
expr :: (Monad m, TokenParsing m) => m Expr
expr = term >>= \t -> 
      Sum t <$> (symbolic '+' *> expr)
  <|> Sub t <$> (symbolic '-' *> expr)
  <|> pure t

term :: (Monad m, TokenParsing m) => m Expr
term = factor >>= \f ->
      Mul f <$> (symbolic '*' *> factor)
  <|> Div f <$> (symbolic '/' *> factor)
  <|> pure f

factor :: (Monad m, TokenParsing m) => m Expr
factor = terminal >>= \t ->
      Exp t <$> (symbolic '^' *> terminal)
  <|> pure t

terminal :: (Monad m, TokenParsing m) => m Expr
terminal = Paren <$> parens expr <|> I <$> natural

run :: String -> Either String Expr
run s = case readP_to_S expr s of
  [] -> Left "Invalid expression"
  xs -> Right . fst . last $ xs