{-# LANGUAGE NoMonomorphismRestriction #-}
module RegularExpressionsTagless where

import RegularExpressionsShallow (Input, Leftovers)
import qualified RegularExpressionsShallow as Sh

class RegExp repr where
    char :: Char -> repr
    conc :: repr -> repr -> repr
    star :: repr -> repr

ex :: RegExp repr => repr
ex = char 'a'

exIntrpd :: String
exIntrpd = ex :: String -- Uses the "RegExp String" instance to interpret 'ex'!

exIntrpd' :: String -- Since we have a top-level type signature, we don't need to also cast the expression below.
exIntrpd' = ex -- Uses the "RegExp String" instance to interpret 'ex'!

instance RegExp Sh.RegExp where
    char = Sh.char
    conc = Sh.conc
    star = Sh.star

instance RegExp String where
    char c = [c]
    conc l r = l ++ r
    star re 
        | null (tail re) = re ++ "*"
        | otherwise = "(" ++ re ++ ")*"

re1 :: RegExp repr => repr
re1 = star (conc (char 'a') (char 'b'))

re2 :: RegExp repr => repr
re2 = char 'c'

re3 :: RegExp repr => repr
re3 = conc re1 re2

-- >>> re1 :: String
-- "(ab)*"

-- >>> Sh.accepts re1 "ab"
-- True

-- >>> Sh.accepts re3 "abc"
-- True

-- >>> re3 :: String
-- "(ab)*c"

class PlusRE repr where
    plus :: repr -> repr

instance PlusRE Sh.RegExp where
    plus re inp = do
        once <- re inp
        star re once

instance PlusRE String where
    plus re
        | null (tail re) = re ++ "+"
        | otherwise = "(" ++ re ++ ")+"

re4 :: (PlusRE repr, RegExp repr) => repr
re4 = plus re3

-- >>> re4 :: String
-- "((ab)*c)+"
