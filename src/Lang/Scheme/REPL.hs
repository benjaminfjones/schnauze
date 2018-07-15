module Lang.Scheme.REPL (
    repl
  , readExpr
) where

import Text.Parsec (parse)

import Lang.Scheme.AST (Expr(..))
import Lang.Scheme.Parser (expr)

-- | TODO
repl :: String -> IO ()
repl input = putStrLn (readExpr input)

-- | Parse an expression, returning either an error message or a success
-- message.
readExpr :: String -> String
readExpr input = case parse expr "REPL" input of
  Left err -> show err
  Right x  -> "read expression: " ++ show x
