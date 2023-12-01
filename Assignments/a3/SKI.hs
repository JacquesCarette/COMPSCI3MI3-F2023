module A3.Sol.SKI (SKI (..), ski, ex1, ex2, ex3, reduce, printReductions) where

type SmallStepReducer e = e -> Maybe e

data SKI where
  S, K, I :: SKI
  App :: SKI -> SKI -> SKI

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App body a@(App _ _)) = show body ++ "(" ++ show a ++ ")"
  show (App body arg) = show body ++ show arg

ski :: SmallStepReducer SKI
ski (App e1 e2) = 
  case ski e1 of
    Just e1' -> Just $ App e1' e2
    Nothing -> 
      case ski e2 of
        Just e2' -> Just $ App e1 e2'
        Nothing -> go (App e1 e2)
  where
    go (App I arg) = Just arg
    go (App (App K x) _) = Just x
    go (App (App (App S x) y) z) = Just $ App (App x z) (App y z)
    go _ = Nothing
ski _ = Nothing

reduce :: SmallStepReducer e -> e -> [e]
reduce ssr e =
  e : case ssr e of
    (Just e') -> reduce ssr e'
    Nothing -> []

printReductions :: (Show e) => SmallStepReducer e -> e -> IO ()
printReductions strat e = mapM_ print (reduce strat e)

-- SKI(KIS)
ex1 :: SKI
ex1 = App (App (App S K) I) (App (App K I) S)

-- KS(I(SKSI)
ex2 :: SKI
ex2 = App (App K S) (App I (App (App (App S K) S) I))

-- SKIK
ex3 :: SKI
ex3 = App (App (App S K) I) K
