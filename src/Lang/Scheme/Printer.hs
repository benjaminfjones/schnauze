-- | A basic pretty printer for scheme expressions; uses the s-cargot library.

module Lang.Scheme.Printer (
    ppExpr
  , translate
) where

import Data.Text (Text, pack)
import Data.SCargot (encode, basicPrint)
import Data.SCargot.Repr (SExpr(..))
-- import Data.SCargot.Language.Basic (basicPrinter)

import Lang.Scheme.AST

translate :: Expr -> SExpr (Text)
translate e = case e of
  Atom t        -> SAtom (pack t)
  List []       -> SNil
  List (x:rest) -> SCons (translate x) (translate (List rest))
  NumberLit x   -> SAtom (pack (show x))
  StringLit x   -> SAtom (pack (show x))
  CharLit x     -> case x of
                     ' ' -> SAtom (pack "#\\space")
                     '\n' -> SAtom (pack "#\\newline")
                     c    -> SAtom (pack ("#\\" ++ [c]))
  BoolLit x     -> SAtom (pack (if x then "#t" else "#f"))

ppExpr :: Expr -> Text
ppExpr e = encode (basicPrint id) [translate e]
