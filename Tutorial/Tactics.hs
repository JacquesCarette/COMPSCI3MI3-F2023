module Tactics where

-- | λ-calculus with booleans and pairs.
data Expr
    = Var String
    -- ^ Variables: 'x'
    | Lam String Expr
    -- ^ Lambdas: 'λ x. e'
    | App Expr Expr
    -- ^ Applcations: 'e e'
    | TT
    -- ^ True: 'true'
    | FF
    -- ^ False: 'true'
    | If Expr Expr Expr
    -- ^ If-then-else: 'if e then e else e'
    | Pair Expr Expr
    -- ^ Pairs: '(e, e)'
    | Fst Expr
    -- ^ First component of pairs: 'fst e'
    | Snd Expr
    -- ^ Second component of pairs: 'snd e'
    deriving (Show, Eq)

-- | Types for the simply-typed λ-calculus
data Type
  = BoolTp
  -- ^ Boolean type: 'Bool'
  | PairTp Type Type
  -- ^ Pair type: '(tp, tp)'
  | FnTp Type Type
  -- ^ Function type: 'tp -> tp'
  deriving (Show, Eq)

-- * A small, shallow embeded DSL for building well-typed expressions in the
-- simply-typed λ-calculus.

-- | A 'Tactic' is a small program that constructs a well-typed, well-scoped
-- expression in the λ-calculus.
--
-- The list of tuples denote what variables are in use along with their types so
-- we can generate fresh names, and we return a 'Just' that contains an
-- expression and type if we could construct a well-typed expression, and
-- 'Nothing' if we encountered a type error.
type BoundVarTypes = [(String, Type)]
type Tactic = [(String, Type)] -> Maybe (Expr, Type)

-- | Check to see if two types are equal.
-- We use 'Maybe ()' here to work nicely with do-notation.
equate :: Type -> Type -> Maybe ()
equate tp tp' | tp == tp = Just ()
              | otherwise = Nothing

-- | Pick a name that isn't found in the provided list of names.
freshen :: String -> [String] -> String
freshen name avoid | name `elem` avoid = freshen (name ++ "'") avoid
                   | otherwise = name

-- | Tactic for constructing 'true'.
tt :: Tactic
tt _ = Just (TT, BoolTp)

-- | Tactic for constructing 'false'.
ff :: Tactic
ff _ = Just (FF, BoolTp)

-- | Tactic for constructing 'if e then e else e'; note that we take in tactics
-- for each of the subexpressions.
ite :: Tactic -> Tactic -> Tactic -> Tactic
ite b t f names = do
    (be, btp) <- b names
    equate btp BoolTp
    (te, ttp) <- t names
    (fe, ftp) <- f names
    equate ttp ftp
    Just (If be te fe, ttp)

-- | Tactic for constructing 'pair e e'.
pair :: Tactic -> Tactic -> Tactic
pair l r names = do
    (le, ltp) <- l names
    (re, rtp) <- r names
    Just (Pair le re, PairTp ltp rtp)

-- | Tactic for constructing 'fst e'.
first :: Tactic -> Tactic
first e names = do
    (ee, etp) <- e names
    case etp of
      (PairTp a b) -> Just (Fst ee, a)
      _ -> Nothing

-- | Tactic for constructing 'snd e'.
second :: Tactic -> Tactic
second e names = do
    (ee, etp) <- e names
    case etp of
      (PairTp a b) -> Just (Snd ee, b)
      _ -> Nothing

-- | Tactic for constructing 'λ x. e'.
-- 
-- We take in a name to bind, and also it's type. Next, we take in a function
-- 'Tactic -> Tactic'; the idea is that we will call this function with a tactic
-- that returns the variable in question.
lam :: String -> Type -> (Tactic -> Tactic) -> Tactic
lam x atp k names = do
    let x' = freshen x (map fst names)
    (be, btp) <- k (\_ -> Just (Var x', atp)) ((x', atp) : names)
    pure (Lam x' be, FnTp atp btp)

-- | Tactic for constructing variables
--
-- Variables are expected to be have their respective types in the original list
-- of name and type signatures provided to a tactic. All of the variables
-- manually constructed in this manner are free in the expression, but bound in
-- the greater scope.
var :: String -> Tactic
var v names = do
    t' <- lookup v names
    Just (Var v, t')

-- | Tactic for construction 'e e'.
app :: Tactic -> Tactic -> Tactic
app fn arg names = do
    (fne, fntp) <- fn names
    (arge, argtp) <- arg names
    case fntp of
      (FnTp atp btp) -> Just (App fne arge, btp)
      _ -> Nothing

-- * Examples

-- | 'not' as a lambda expression.
enot :: Tactic
enot = lam "x" BoolTp $ \x ->
  ite x ff tt

-- | Example showing that we handle shadowing correctly.
shadows :: Tactic
shadows =
    lam "x" BoolTp $ \x ->
    lam "x" BoolTp $ \y -> x

-- >>> var "x" []
-- Nothing

-- >>> var "x" [("x", FnTp BoolTp BoolTp)]
-- Just (Var "x",FnTp BoolTp BoolTp)

-- >>> first (pair (var "x") (var "y")) [("x", BoolTp), ("y", BoolTp)]
-- Just (Fst (Pair (Var "x") (Var "y")),BoolTp)
