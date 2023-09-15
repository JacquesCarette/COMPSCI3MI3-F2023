-- September 14 Lecture

-- Learning Objectives:
-- - understand how more features work
-- - make life easier with types

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Sept14 where

-- Let's start from this typed language
data Expr a where
  Int :: Integer -> Expr Integer
  Add :: Expr Integer -> Expr Integer -> Expr Integer
  Mul :: Expr Integer -> Expr Integer -> Expr Integer
  Minus :: Expr Integer -> Expr Integer
  B :: Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool
  Implies :: Expr Bool -> Expr Bool -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  IF :: Expr Bool -> Expr b -> Expr b -> Expr b
  Pair :: Expr a -> Expr b -> Expr (a,b)
  Fst  :: Expr (a, b) -> Expr a
  Snd  :: Expr (a, b) -> Expr b
-- can't derive Show anymore!

-- an evaluator
eval :: Expr a -> a
eval (Int a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Minus a) = - (eval a)
eval (B b) = b
eval (Or a b) = eval a || eval b
eval (Implies a b) = (not $ eval a) || eval b
eval (Not a) = not $ eval a
eval (IF cd th el) = if eval cd then eval th else eval el
eval (Pair x y) = (eval x , eval y)
eval (Fst x) = fst $ eval x
eval (Snd x) = snd $ eval x

parens :: String -> String
parens x = "(" ++ x ++ ")"

show_infix2 :: (Show a, Show b) => a -> b -> String -> String
show_infix2 x y i = parens $ show x ++ i ++ show y

-- Let's write a very primitive show
instance Show (Expr a) where
  show (Int a) = parens $ show a
  show (B b) = show b
  --
  show (Add a b)     = show_infix2 a b " + "
  show (Mul a b)     = show_infix2 a b " * "
  show (Or a b)      = show_infix2 a b " || "
  show (Implies a b) = show_infix2 a b " ==> "
  --
  show (Minus a) = parens $ "- "   ++ show a
  show (Not a)   = parens $ "not " ++ show a
  show (Fst a)   = parens $ "fst " ++ show a
  show (Snd a)   = parens $ "snd " ++ show a
  --
  show (IF cd th el) = parens $ 
    "if " ++ show cd ++ 
    "then " ++ show th ++
    "else " ++ show el
  --
  show (Pair x y) = show_infix2 x y " , "

-- Then let's add features:
-- 1. pairs (including swap)
-- 2. lists
-- 3. unit
-- 4. choice of 2

-- if there's time: 5. functions
