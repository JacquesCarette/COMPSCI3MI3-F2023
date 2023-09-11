-- September 12 Lecture

-- Learning Objectives:
-- - refresh on Haskell
-- - operational semantics via interpreters
-- - some ideas about A1

module Sept12 where

-- A tiny language of integer expressions
data IExpr =
    Int Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  | Minus IExpr

-- an evaluator
evalIE :: IExpr -> IExpr
evalIE x = undefined

-- a better evaluator
evalII :: IExpr -> Integer
evalII x = undefined

------------------------------
-- Write down some BNF here
-- Write down some big-step semantics here

-- A tiny language of booleans
data BExpr =
    B Bool
  | Or BExpr BExpr
  | Implies BExpr BExpr
  | Not BExpr

-- an evaluator for this
evalB :: BExpr -> Bool
evalB x = undefined

-- A sneaky way to combine them
data Both =
    IntE IExpr
  | BoolE BExpr
  | IF BExpr IExpr IExpr

-- an evaluator for that too
evalBoth :: Both -> Either Bool Integer
evalBoth x = undefined

------------------------------
-- If there is time, implement a printer
