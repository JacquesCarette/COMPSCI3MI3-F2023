module A3.LoopyLambda where

import Data.Map (Map)
import Data.Set (Set)

import Data.Map qualified as Map
import Data.Set qualified as Set

-- | Syntax for the loopy lambda language.
data Expr
    = Var String
    -- ^ Variables: 'x'
    | Lam String Expr
    -- ^ Lambdas: 'λ x. e'
    | App Expr Expr
    -- ^ Application: 'e e'
    | Zero
    -- ^ Zero: '0'
    | PlusOne Expr
    -- ^ Successor: '1 + e'
    | Loop Expr Expr Expr
    -- ^ Loops: 'loop e e e'
    deriving (Show)

-- * Operations on variables

-- | Compute the set of free variables of an expression.
freeVars :: Expr -> Set String
freeVars (Var x) = Set.singleton x
freeVars (Lam x e) = Set.delete x (freeVars e)
freeVars (App e1 e2) = Set.union (freeVars e1) (freeVars e2)
freeVars Zero = Set.empty
freeVars (PlusOne e) = freeVars e
freeVars (Loop e1 e2 e3) = Set.unions [freeVars e1, freeVars e2, freeVars e3]

-- | Compute the set of all variables of an expression, free or bound.
allVars :: Expr -> Set String
allVars (Var x) = Set.singleton x
allVars (Lam x e) = Set.insert x (freeVars e)
allVars (App e1 e2) = Set.union (freeVars e1) (freeVars e2)
allVars Zero = Set.empty
allVars (PlusOne e) = allVars e
allVars (Loop e1 e2 e3) = Set.unions [allVars e1, allVars e2, allVars e3]

-- | Pick a name that isn't found in the provided set of names.
freshen :: String -> Set String -> String
freshen name avoid | Set.member name avoid = freshen (name ++ "'") avoid
                   | otherwise = name

-- | 'transpose x y a' will swap 'a' if it is 'x' or 'y', and leave it unchanged otherwise
transpose :: String -> String -> String -> String
transpose x y a | a == x = y
                | a == y = x
                | otherwise = a

-- | Swap the names 'x' and 'y' in an expression.
swapNames :: String -> String -> Expr -> Expr
swapNames x y (Var v) = Var (transpose x y v)
swapNames x y (Lam v e) = Lam (transpose x y v) (swapNames x y e)
swapNames x y (App e1 e2) = App (swapNames x y e1) (swapNames x y e2)
swapNames _ _ Zero = Zero
swapNames x y (PlusOne e) = PlusOne (swapNames x y e)
swapNames x y (Loop e1 e2 e3) = Loop (swapNames x y e1) (swapNames x y e2) (swapNames x y e3)

-- | Rename a term to not use the names in the provided list.
rename :: Expr -> Set String -> Expr                   
rename e avoid = go e Map.empty
  where
    -- Basic algorithm is to track what we have renamed variables
    -- to, and then freshen each bound variable.
    go :: Expr -> Map String String -> Expr
    go (Var x) rn =
        case Map.lookup x rn of
          Just y -> Var y
          Nothing -> Var x
    go (Lam x e) rn =
        -- Invent a new name for 'x', and then record
        -- it's new name.
        let x' = freshen x avoid in
        Lam x' (go e (Map.insert x x' rn))
    go (App e1 e2) rn =
        App (go e1 rn) (go e2 rn)
    go Zero _ =
        Zero
    go (PlusOne e) rn =
        PlusOne (go e rn)
    go (Loop n s f) rn =
        Loop (go n rn) (go s rn) (go f rn)
                   

-- * Comparing expressions for equality
--
-- We have not included an 'Eq' instance of purpose:
-- consider the two terms 'λ a b. a' and 'λ x y. x'
-- These two terms have the same structure, but different names.
-- We call such terms α-equivalent, and it is the correct notion
-- of equality to use for expressions in the λ-calculus.
-- If you want to compare expressions for equality, use 'alphaEq'.
-- | Compare two terms up to α-equivalence.
alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v1) (Var v2) = v1 == v2
alphaEq (Lam v1 e1) (Lam v2 e2) =
    -- Pick a name that doesn't occur anywhere in 'e1' or 'e2',
    -- replace all occurances of 'v1' and 'v2' with this new name, then
    -- keep comparing for α-equivalence.
    let v' = freshen "x" (Set.unions [Set.fromList [v1, v2], allVars e1, allVars e2])
    in alphaEq (swapNames v' v1 e1) (swapNames v' v2 e2)
alphaEq (App f1 a1) (App f2 a2) =
    alphaEq f1 f2 && alphaEq a1 a2
alphaEq Zero Zero = True
alphaEq (PlusOne e1) (PlusOne e2) =
    alphaEq e1 e2
alphaEq (Loop n1 s1 f1) (Loop n2 s2 f2) = 
    alphaEq n1 n2 && alphaEq s1 s2 && alphaEq f1 f2
alphaEq _ _ = False

-- * Capture-avoiding substitution

-- | Substitute every occurance of 'e1' for 'x' in 'e2'.
subst :: String -> Expr -> Expr -> Expr
subst x e1 e2 = substRenamed x e1 (rename e2 (freeVars e1))
  where
    substRenamed :: String -> Expr -> Expr -> Expr
    substRenamed x _ (Var v) | v == x = e1
                              | otherwise = Var v
    substRenamed x e (Lam v body) | v == x = Lam v body
                                  | otherwise = Lam v (substRenamed x e body)
    substRenamed x e (App f a) = App (substRenamed x e f) (substRenamed x e a)
    substRenamed _ _ Zero = Zero
    substRenamed x e (PlusOne n) = PlusOne (substRenamed x e n)
    substRenamed x e (Loop n s f) =
        Loop (substRenamed x e n) (substRenamed x e s) (substRenamed x e f)

-- * Question 2.1:

stepLoop :: Expr -> Maybe Expr
stepLoop = error "Your code goes here!"

-- * Question 2.2:
-- Either implement the following function using the extra reduction rule,
-- or describe why we cannot implement it in the comment block below.

stepLoopExtra :: Expr -> Maybe Expr
stepLoopExtra = error "Your code goes here! (if possible)"


{- [Question 2.2]:
   <your answer>
-}
