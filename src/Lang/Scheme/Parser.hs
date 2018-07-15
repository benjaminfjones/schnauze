module Lang.Scheme.Parser (
  expr
) where

import Text.Parsec

import Lang.Scheme.AST

type Parser = Parsec String ()

expr :: Parser Expr
expr = choice [atom, list, stringLit, numberLit, boolLit]

-- | Parse an atom.
atom :: Parser Expr
atom = fail "atom"

-- | Parse a list of expressions.
--
-- TODO doesn't handle whitespace properly
list :: Parser Expr
list = List <$> between (char '(') (char ')') (sepBy expr spaces1)

-- | Parse a string literal.
--
-- A string literal is a sequence of zero or more non-quote characters between
-- quotes.
stringLit :: Parser Expr
stringLit = do
  let q = char '"'
  s <- between q q (many (noneOf "\""))
  return $ StringLit s

-- | Parse an integer.
--
-- This is done by parsing a non-empty sequence of digits and then calling
-- Haskell's 'read' at the 'Integer' type.
numberLit :: Parser Expr
numberLit = do
  numStr <- many1 digit
  return $ NumberLit (read numStr :: Integer)

-- | Parse a boolean literal @#t@ or @#f@.
boolLit :: Parser Expr
boolLit = do
  _ <- char '#'
  c <- oneOf "tf"
  return $ BoolLit (c == 't')

-- | Parse one or more spaces.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- | Parse a valid symbol.
symbol :: Parser Char
symbol = spaces1 >> oneOf "!#$%&|*+-/:<=>?@^_~"
