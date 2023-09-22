module Sept22 where

qs :: [Integer] -> [Integer]
qs [] = []
qs (x : xs) = 
  let (left, right) = partition xs x in
  qs left ++ (x : qs right)

partition :: [Integer] -> Integer -> ([Integer], [Integer])
partition [] x = ([],  [])
partition (y : ys) x | y < x = 
  let p = partition ys x in
  (y : fst p, snd p)
partition (y : ys) x | y >= x = 
  let p = partition ys x in
  (fst p, y : snd p)

listsum2 :: [Integer] -> Integer
listsum2 = foldr (+) 0

listlen2 :: [Integer] -> Integer
listlen2 = foldr (\_ l -> l + 1) 0
