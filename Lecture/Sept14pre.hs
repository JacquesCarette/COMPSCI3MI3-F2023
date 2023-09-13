-- September 14 Lecture

-- Learning Objectives:
-- - understand how more features work
-- - make life easier with types

{-# OPTIONS_GHC -Wall #-}
module Sept14 where

-- Let's start from this typed language
data Expr a where
  Int :: Integer -> Expr Integer
  Add :: Expr Integer -> Expr Integer -> Expr Integer
  Mul :: Expr Integer -> Expr Integer -> Expr Integer
  Minus :: Expr Integer -> Expr Integer
  B :: Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool
  Implies :: Expr Bool -> Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool -> Expr Bool
  IF :: Expr Bool -> Expr a -> Expr a -> Expr a
-- can't derive Show anymore!

-- an evaluator
eval :: Expr a -> a
eval _ = undefined

-- Let's write a very primitive show
instance Show (Expr a) where
  show _ = undefined

-- Then let's add features:
-- 1. pairs (including swap)
-- 2. lists
-- 3. unit
-- 4. choice of 2

-- if there's time: 5. functions
