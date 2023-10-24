{-# OPTIONS_GHC -Wall #-}
module Oct19 where

-- this doesn't have a reasonable type
fix f = (\ x -> f (\ y -> x x y)) (\ x -> f (\ y -> x x y))
