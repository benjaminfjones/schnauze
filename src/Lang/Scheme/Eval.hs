module Lang.Scheme.Eval (
) where

import Lang.Scheme.AST

eval :: Expr -> Expr
eval val@(StringLit _) = val
eval val@(CharLit _) = val
eval val@(NumberLit _) = val
eval val@(BoolLit _) = val
eval (List [Atom "quote", val]) = val
eval val = error ("eval: case not yet implemented: " ++ show val)
