module Lang.Scheme.Parser (
  expr
) where

import Control.Monad (void)
import Data.Text (Text)
import Text.Parsec

import Lang.Scheme.AST

type Parser = Parsec Text ()

-- | Parse a scheme expression.
--
-- Note: the boolean literal case is handled by 'atom'.
expr :: Parser Expr
expr = choice [ atom
              , list
              , quoted
              , quasiquoted
              , unquoted
              , stringLit
              , charLit
              , numberLit]
       <?> "expression"

-- | Parse an atom.
--
-- An atom is an identifier starting with a letter or symbol, followed by any
-- number of letters, symbols, or digits.
--
-- Some special constant literals must be handled here like the booleans @#t@ and @#f@.
atom :: Parser Expr
atom = do
  a <- try $ do
         h <- letter <|> symbol
         t <- many (letter <|> symbol <|> digit)
         lookAhead delimeter
         return (h:t)
  -- TODO: Keywords
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
  s <- between quote quote (many strChar)
  return $ StringLit s

  where

  strChar =  noneOf "\\\""
         <|> esc quote
         <|> esc (char 'n')
         <|> esc (char 'r')
         <|> esc (char 't')
         <|> esc (char '\\')
         <?> "valid string character"

-- | Parse a character literal.
--
-- Examples: #\a       --> 'a'
--           #\Z       --> 'Z'
--           #\        --> ' '
--           #\space   --> ' '
--           #\newline --> '\n'
--
charLit :: Parser Expr
charLit = do
  _ <- string "#\\"
  delimeterCase <|> letterCase <|> nameCase <?> "character literal"
  where
  delimeterCase = lookAhead delimeter >> return (CharLit ' ')
  letterCase    = try $ do
    c <- letter
    lookAhead delimeter
    return (CharLit c)
  nameCase      = try $ do
    h <- letter
    t <- many1 letter
    lookAhead delimeter
    let name = h:t
    r <- case name of
           "space"   -> return (CharLit ' ')
           "newline" -> return (CharLit '\n')
           _         -> unexpected ("character literal name: " ++ name)
    return r

-- | Parse an integer.
--
-- This is done by parsing a non-empty sequence of digits and then calling
-- Haskell's 'read' at the 'Integer' type.
--
-- TODO: support different bases
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

-- | Parse a quoted expression.
quoted :: Parser Expr
quoted = do
  char '\''
  e <- expr
  return (wrapExpr "quote" e)

-- | Parse a quasiquoted expression.
quasiquoted :: Parser Expr
quasiquoted = do
  char '`'
  e <- expr
  return (wrapExpr "quasiquote" e)

-- | Parse an unquote/unquote-splice expression.
unquoted :: Parser Expr
unquoted = do
  char ','
  -- TODO: order of these is fragile
  spliceCase <|> unquoteCase
  where
  unquoteCase = do
    e <- expr
    return (wrapExpr "unquote" e)
  spliceCase = do
    char '@'
    e <- expr
    return (wrapExpr "unquote-splicing" e)

wrapExpr :: String -> Expr -> Expr
wrapExpr ident e = List [Atom ident, e]


-- Helpers ---------------------------------------------------------------------


quote :: Parser Char
quote = char '"'

delimeter :: Parser ()
delimeter = void (oneOf " ()") <|> eof

esc :: Parser Char -> Parser Char
esc p = char '\\' >> p
