module Lang.Scheme.Parser (
  expr
) where

import Text.Parsec

import Lang.Scheme.AST

type Parser = Parsec String ()

-- | Parse a scheme expression.
--
-- Note: the boolean literal case is handled by 'atom'.
expr :: Parser Expr
expr = choice [atom, list, stringLit, numberLit]

-- | Parse an atom.
--
-- An atom is an identifier starting with a letter or symbol, followed by any
-- number of letters, symbols, or digits.
--
-- Some special constant literals must be handled here like the booleans @#t@ and @#f@.
atom :: Parser Expr
atom = do
  h <- letter <|> symbol
  t <- many (letter <|> symbol <|> digit)
  let a = h:t
  return $ case a of
             "#t" -> BoolLit True
             "#f" -> BoolLit False
             _    -> Atom a

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
  s <- between quote quote (many notUnescapedQuote)
  return $ StringLit s

  where

  notUnescapedQuote =  noneOf "\\\""
                   <|> (char '\\' >> quote)

-- | Parse an integer.
--
-- This is done by parsing a non-empty sequence of digits and then calling
-- Haskell's 'read' at the 'Integer' type.
numberLit :: Parser Expr
numberLit = do
  numStr <- many1 digit
  return $ NumberLit (read numStr :: Integer)

-- | Parse one or more spaces.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- | Parse a valid symbol.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- Helpers ---------------------------------------------------------------------


quote :: Parser Char
quote = char '"'
