{-# LANGUAGE OverloadedStrings #-}
module Lang.Scheme.REPL (
    repl
  , readExpr
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec (parse)

import Lang.Scheme.AST (Expr(..))
import Lang.Scheme.Parser (expr)
import Lang.Scheme.Printer (ppExpr)

-- | TODO
repl :: Text -> IO ()
repl input = T.putStrLn (readExpr input)

-- | Parse an expression, returning either an error message or a success
-- message.
readExpr :: Text -> Text
readExpr input = case parse expr "REPL" input of
  Left err -> "readExpr error: " <> T.pack (show err)
  Right x  -> "read expression:\n" <> ppExpr x <> "\n"
