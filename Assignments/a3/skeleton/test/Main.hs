module Main (main) where

import Test.HUnit
import System.Exit

-- Run the test suite. Please do not touch this!
main :: IO ()
main = do
    counts <- runTestTT a1Tests
    if errors counts + failures counts == 0 then
      exitSuccess
    else
      exitFailure
