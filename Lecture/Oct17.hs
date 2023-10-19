{-# OPTIONS_GHC -Wall #-}
module Oct17 where

tru :: b -> b -> b
fls :: b -> b -> b
tru t _ = t
fls _ f = f

pair :: a -> b -> (a -> b -> c) -> c
pair f s p = p f s

fst :: ((b -> b -> b) -> t) -> t
fst p = p tru
snd :: ((b -> b -> b) -> t) -> t
snd p = p fls
