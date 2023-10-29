-- |
module LoopyLambdaTests (lambdaTests) where

import A3.LoopyLambda

import Test.HUnit

-- | Helper for testing α-equivalence of expressions.
assertAlphaEqual :: String -> Expr -> Expr -> Assertion
assertAlphaEqual msg e1 e2 = assertBool msg (alphaEq e1 e2)

-- These tests are meant to serve as examples for how to use 'HUnit'.
-- !!!! THESE DO NOT COUNT TOWARDS YOUR MARKS, AND SHOULD BE REMOVED BEFORE SUBMISSION !!!!
-- TODO: Delete these tests, and write your own for 'Expr'.
lambdaTests :: Test
lambdaTests = TestList
  [ TestCase (assertAlphaEqual "Example test, please delete" (Lam "a" (Lam "b" (Var "a"))) (Lam "x" (Lam "y" (Var "x"))))
  ]
