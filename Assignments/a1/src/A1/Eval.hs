-- | Evaluator for Assignment 1.
module A1.Eval where

data Expr =
    EInt Integer
  | ECh Char
  | EBool Bool
  | EString String
  --
  | EAdd Expr Expr    -- addition of integers
  | EMul Expr Expr    -- multiplication of integers
  | ENeg Expr         -- negation of integers
  | ECat Expr Expr    -- concatenation of strings
  | ECons Char Expr   -- adding a character at the start of a string
  | EAnd Expr Expr    -- AND of two booleans
  | EXor Expr Expr    -- XOR of two booleans
  | EIf Expr Expr Expr -- if-then-else
  | EShowInt Expr      -- render an integer as a string

data Val =
    VInt Integer
  | VBool Bool
  | VString String
  | VError             -- something went wrong

evalExpr :: Expr -> Val
evalExpr = error "TODO: Your code lives here!"
