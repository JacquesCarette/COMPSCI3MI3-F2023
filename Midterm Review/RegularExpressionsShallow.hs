module RegularExpressionsShallow where

type Input = String
type Leftovers = Maybe String

type RegExp = Input -> Leftovers

char :: Char -> RegExp
char t (c:cs)
  | t == c    = Just cs
  | otherwise = Nothing
char _ [] = Nothing

conc :: RegExp -> RegExp -> RegExp
conc l r s = do
    l' <- l s
    r l'

star :: RegExp -> RegExp
star _  [] = Just []
star re cs = maybe (Just cs) (star re) (re cs)

-- Note: plus is an after-thought, easily extend RegExp language
-- plus :: RegExp -> RegExp
-- plus re cs = do
--   cs' <- re cs -- consume at least once
--   star re cs'

-- extensions are built with extra shallow-embedded functions

accepts :: RegExp -> Input -> Bool
accepts re inp = re inp == Just []

-- >>> accepts (char 'a') "a"
-- True

-- >>> accepts (conc (char 'a') (char 'b')) "a"
-- False

-- >>> accepts (conc (char 'a') (char 'b')) "b"
-- False

-- >>> accepts (conc (char 'a') (char 'b')) "ab"
-- True

-- >>> accepts (star (conc (char 'a') (char 'b'))) ""
-- True

-- >>> accepts (star (conc (char 'a') (char 'b'))) "ab"
-- True

-- >>> accepts (star (conc (char 'a') (char 'b'))) "ababab"
-- True

-- >>> accepts (star (conc (char 'a') (char 'b'))) "ababababababba"
-- False
