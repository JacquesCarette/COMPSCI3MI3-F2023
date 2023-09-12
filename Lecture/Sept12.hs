-- September 12 Lecture

-- Learning Objectives:
-- - refresh on Haskell
-- - operational semantics via interpreters
-- - some ideas about A1

{-# OPTIONS_GHC -Wall #-}
module Sept12 where

-- A tiny language of integer expressions
data IExpr =
    Int Integer      -- Integer literals
  | Add IExpr IExpr  -- addition
  | Mul IExpr IExpr  -- multiplication
  | Minus IExpr      -- unary minus
  deriving (Show)

{-
-- an evaluator
evalIE :: IExpr -> IExpr
evalIE (Int i) = Int i
evalIE (Add a b) = 
  let Int i = evalIE a in
  let Int j = evalIE b in
  Int (i + j)
evalIE (Mul a b) =
  let Int i = evalIE a in
  let Int j = evalIE b in
  Int (i * j)
evalIE (Minus a) =
  let Int i = evalIE a in
  Int (-i)
-}
-- a better evaluator
-- Intent is what matches (Add, +), (Mul, +)
evalII :: IExpr -> Integer
evalII (Int i)   = i
evalII (Add a b) = evalII a + evalII b
evalII (Mul a b) = evalII a * evalII b
evalII (Minus a) = - evalII a
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
evalB (B b) = b
evalB (Or a b) = evalB a || evalB b
evalB (Implies a b) = (not $ evalB a) || evalB b
evalB (Not a)       = not (evalB a)

-- A sneaky way to combine them
data Both =
    IntE IExpr
  | BoolE BExpr
  | IF BExpr IExpr IExpr

-- an evaluator for that too
evalBoth :: Both -> Either Bool Integer
evalBoth (IntE a) = Right $ evalII a
evalBoth (BoolE b) = Left $ evalB b
evalBoth (IF b x y) = Right $
  if (evalB b) 
  then evalII x
  else evalII y

------------------------------
-- If there is time, implement a printer
