module RegularExpressionsDeep where

import RegularExpressionsShallow (Input, Leftovers)
import qualified RegularExpressionsShallow as Sh

-- Note: + is an after-thought
data RegExp where
    Char :: Char -> RegExp
    Conc :: RegExp -> RegExp -> RegExp
    Star :: RegExp -> RegExp
    -- Plus :: RegExp -> RegExp

-- | "to shallow"
toSh :: RegExp -> Sh.RegExp
toSh (Char c)   = Sh.char c
toSh (Conc l r) = Sh.conc (toSh l) (toSh r)
toSh (Star re)  = Sh.star (toSh re)
-- toSh (Plus re) = let re' = toSh re 
--                   in Sh.conc re' (Sh.star re')

accepts :: RegExp -> Input -> Bool
accepts re inp = toSh re inp == Just []

instance Show RegExp where
    show (Char c) = [c]
    show (Conc l r) = show l ++ show r
    show (Star re) = let re' = show re
                     in if null (tail re')
                        then re' ++ "*"
                        else "(" ++ re' ++ ")*"
    -- show (Plus re) = let re' = show re
    --                  in if null (tail re')
    --                     then re' ++ "+"
    --                     else "(" ++ re' ++ ")+"

-- >>> show (Char 'a')
-- "a"

-- >>> show (Conc (Char 'a') (Char 'b'))
-- "ab"

-- >>> show (Star (Conc (Char 'a') (Char 'b')))
-- "(ab)*"

-- >>> accepts (Char 'a') "a"
-- True

-- >>> accepts (Conc (Char 'a') (Char 'b')) "a"
-- False

-- >>> accepts (Conc (Char 'a') (Char 'b')) "b"
-- False

-- >>> accepts (Conc (Char 'a') (Char 'b')) "ab"
-- True

-- >>> accepts (Star (Conc (Char 'a') (Char 'b'))) ""
-- True

-- >>> accepts (Star (Conc (Char 'a') (Char 'b'))) "ab"
-- True

-- >>> accepts (Star (Conc (Char 'a') (Char 'b'))) "ababab"
-- True

-- >>> accepts (Star (Conc (Char 'a') (Char 'b'))) "ababababababba"
-- False
