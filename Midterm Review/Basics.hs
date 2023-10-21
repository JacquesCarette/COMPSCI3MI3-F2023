module Basics where

import Prelude hiding (elem)

elem :: Eq a => a -> [a] -> Bool
elem e (x:xs) = e == x || elem e xs
elem _ []     = False

-------------------------------------------------------------------------------
-- Binary trees
-------------------------------------------------------------------------------

-- data Mabe a = Nothing | Just a

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

-------------------------------------------------------------------------------
-- Binary trees
-------------------------------------------------------------------------------

data BT = Leaf Int | Branch BT BT

hasLeaf :: Int -> BT -> Bool
hasLeaf t (Leaf x)     = t == x
hasLeaf t (Branch l r) = hasLeaf t l || hasLeaf t r

sumLeaves :: BT -> Int
sumLeaves (Leaf x) = x
sumLeaves (Branch l r) = sumLeaves l + sumLeaves r
