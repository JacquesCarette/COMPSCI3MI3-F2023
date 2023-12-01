module A3.Sol.LoopyLambda (stepLoop) where

import A3.LoopyLambda hiding (stepLoop)

stepLoop :: Expr -> Maybe Expr
stepLoop (Var _) = Nothing
stepLoop (Lam _ _) = Nothing
stepLoop Zero = Nothing
stepLoop (App (Lam x e1) e2) = Just $ subst x e2 e1
stepLoop (App body arg) = do
  body' <- stepLoop body
  Just (App body' arg)
stepLoop (PlusOne e) = do
  e' <- stepLoop e
  Just (PlusOne e')
stepLoop (Loop Zero s _) = Just s
stepLoop (Loop i s f) = 
  case i of
    PlusOne ib ->
      case stepLoop ib of
        Just ib' -> Just $ Loop (PlusOne ib') s f
        Nothing  -> Just $ App f (Loop ib s f)
    _ -> do
      i' <- stepLoop i
      Just (Loop i' s f)
