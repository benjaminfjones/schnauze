module Lang.Scheme.AST (
  Expr(..)
) where

-- | A Scheme expression
data Expr = Atom String        -- ^ string names the atom
          | List [Expr]        -- ^ list of expressions
          | NumberLit Integer  -- ^ an integer literal
          | StringLit String   -- ^ a string literal
          | BoolLit Bool       -- ^ a boolean literal
  deriving (Eq, Show)
