-- | Tests for Assignment 1.
module Main where

import A1.Eval

import Test.HUnit
import System.Exit

-- These tests are meant to serve as examples for how to use 'HUnit'.
-- !!!! THESE DO NOT COUNT TOWARDS YOUR MARKS, AND SHOULD BE REMOVED BEFORE SUBMISSION !!!!
-- TODO: Delete these tests, and write your own for 'evalExpr'.
a1Tests :: Test
a1Tests = TestList
  [ TestCase (assertEqual "This is a working test" (reverse "hello") "olleh")
  , TestCase (assertEqual "This is a broken test" (1 + 1) 3)
  ]

-- Run the test suite. Please do not touch this!
main :: IO ()
main = do
    counts <- runTestTT a1Tests
    if errors counts + failures counts == 0 then
      exitSuccess
    else
      exitFailure
