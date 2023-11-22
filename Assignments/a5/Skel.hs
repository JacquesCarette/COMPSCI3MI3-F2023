{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-name-shadowing -Wno-unused-matches #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Skel where

import Control.Applicative

import Data.Functor
import Data.List
import Debug.Trace

-- * Core Data Types
-- TODO: Implement the following data types, following
-- the relevant sections from "Types and Programming Languages" by Benjamin Pierce.

-- | Untyped expressions
data Expr
    = Var String
    -- ^ Variables
    | Lam String Expr | App Expr Expr
    -- ^ Lambda and application (See figure 9-1)
    | Unit
    -- ^ Unit (See 11.2 in TAPL)
    | Let String Expr Expr
    -- ^ Let-bindings (See 11.5 in TAPL)
    | Pair Expr Expr | Fst Expr | Snd Expr
    -- ^ Pairs (See 11.6 in TAPL)
    | TT | FF | If Expr Expr Expr
    -- ^ Booleans (See 8.2 in TAPL)
    | Zero | Succ Expr | Pred Expr | IsZero Expr
    -- ^ Natural Numbers (See 8.2 in TAPL)
    deriving (Show)

-- | Types.
data Type
  = TpVar String
  -- ^ Type variables
  | FnTp Type Type
  -- ^ Function types
  | UnitTp
  -- ^ Unit (See 11.2 in TAPL)
  | PairTp Type Type
  -- ^ Pair Types (See 11.6 in TAPL)
  | BoolTp
  -- ^ Booleans (See 8.2 in TAPL)
  | NatTp
  -- ^ Booleans (See 8.2 in TAPL)
  deriving (Eq, Show)

-- | Typed expressions
data TypedExpr
    = TVar String
    -- ^ Variables
    | TLam String Type TypedExpr | TApp TypedExpr TypedExpr
    -- ^ Lambda and application (See figure 9-1)
    | TUnit
    -- ^ Unit (See figure 11-2 in TAPL)
    | TLet String Scheme TypedExpr TypedExpr
    -- ^ Let-bindings (See figure 11-4 in TAPL)
    | TPair TypedExpr TypedExpr | TFst TypedExpr | TSnd TypedExpr
    -- ^ Pairs (See figure 11-5 in TAPL)
    | TTT | TFF | TIf TypedExpr TypedExpr TypedExpr
    -- ^ Booleans (See figure 8-2 in TAPL)
    | TZero | TSucc TypedExpr | TPred TypedExpr | TIsZero TypedExpr
    -- ^ Natural Numbers (See figure 8-2 in TAPL)
    deriving (Show)

data Scheme = Poly [String] Type | Mono Type
    deriving (Show)

-- | Contexts
type Ctx = [(String, Scheme)]

-- | Substitutions
type Substitution = [(String, Type)]

freeVars :: Expr -> [String]
freeVars e = undefined

freeTpVars :: Type -> [String]
freeTpVars t = undefined

freeSchemeVars :: Scheme -> [String]
freeSchemeVars s = undefined

freeCtxVars :: Ctx -> [String]
freeCtxVars = undefined
-- * Substitution

composeSub :: Substitution -> Substitution -> Substitution
composeSub sub1 sub2 = undefined

composeSubs :: [Substitution] -> Substitution
composeSubs = undefined

-- | Remove a set of variables from a substitution.
thinSub :: [String] -> Substitution -> Substitution
thinSub avoid sub = undefined

-- | Pick a name that isn't found in the provided list of names.
freshen :: String -> [String] -> String
freshen name avoid = undefined
                   
-- | Rename a term to not use the names in the provided list.
rename :: Expr -> [String] -> Expr
rename e = go e []
  where
    -- Basic algorithm is to track what we have renamed variables
    -- to, and then freshen each bound variable.
    go :: Expr -> [(String, String)] -> [String] -> Expr
    go e l1 l2 = undefined

-- | 'subst e1 e2 x' is 'e1[e2/x]'
subst :: Expr -> Expr -> String -> Expr
subst e1 e2 x = undefined

-- | Substitute for type variables in a type.
substTp :: Substitution -> Type -> Type
substTp sub t = undefined

-- | Substitute for type variables in a type scheme.
substScheme :: Substitution -> Scheme -> Scheme
substScheme sub s = undefined

-- | Substitute for type variables in a typed expression.
substTypedExpr :: Substitution -> TypedExpr -> TypedExpr
substTypedExpr sub te = undefined

substCtx :: Substitution -> Ctx -> Ctx
substCtx sub c = undefined

-- * Type Inference and Erasure

generalize :: Ctx -> Type -> Scheme
generalize gamma tp = undefined

instantiate :: Scheme -> Infer Type
instantiate s = undefined

synth :: Ctx -> Expr -> Infer (TypedExpr, Type, Substitution)
synth gamma e = undefined

unify :: Type -> Type -> Infer Substitution
unify t tp = undefined

-- | Infer the type of an expression, returning
-- it's typed counterpart, along with it's type.
infer :: Ctx -> Expr -> Either String (TypedExpr, Type)
infer gamma e = undefined

-- | Erase a typed expression.
erase :: TypedExpr -> Expr
erase te = undefined

step :: Expr -> Maybe Expr
step e = undefined

eval :: Expr -> Expr
eval e = undefined

-- * Inference Monad
-- When we are inferring types, we need to be able to generate fresh names
-- and throw errors. Both of these effects can be captured by monads, but
-- combining monads is a bit tricky. Therefore, we just define a custom
-- monad that does both!

newtype Infer a = Infer { runInfer :: [String] -> Either String (a, [String]) }
    deriving (Functor)

instance Applicative Infer where
    pure a = Infer $ \s -> Right (a, s)
    mf <*> ma = Infer $ \s -> do
        (f, s') <- runInfer mf s
        (a, s'') <- runInfer ma s'
        pure (f a, s'')

instance Monad Infer where
    return = pure
    mf >>= k = Infer $ \s -> do
        (a, s') <- runInfer mf s
        runInfer (k a) s'

-- | Generate a fresh type variable.
freshTpVar :: Infer Type
freshTpVar = Infer $ \names ->
  let name = head names
  in Right (TpVar name, tail names)

-- | Emit a type error.
typeError :: String -> Infer a
typeError err = Infer $ \_ -> Left err

-- | Infinite list of fresh names.
genNames :: [String]
genNames = map pure ['a' .. 'z'] ++ map (++ "'") genNames

execInfer :: Infer a -> Either String a
execInfer m = fst <$> runInfer m genNames

-- * Misc. Helpers

unionWith :: (Eq b) => (a -> [b]) -> [a] -> [b]
unionWith k = foldr (\x bs -> union (k x) bs) []

remove :: (Eq a) => a -> [(a,b)] -> [(a,b)]
remove a [] = []
remove a ((a', x):xs) | a == a' = remove a xs
                      | otherwise = (a', x) : remove a xs
