module Interpreter where

import Lexer
import Parser

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
isValue (Lam _ _ _) = True
isValue _ = False

subst :: String -> Expr -> Expr -> Expr
subst x s y@(Var v) =
  if x == v
    then
      s
    else
      y
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue
subst x s BFalse = BFalse
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2)
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2)
-- Completar subst para outros termos da linguagem
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (Paren e) = Paren (subst x s e)
subst x s (If e1 e2 e3) = If (subst x s e1) (subst x s e2) (subst x s e3)
-- Avoid capture: do not substitute into body when lambda binds same var
subst x s lam@(Lam y tp t1) = if x == y then lam else Lam y tp (subst x s t1)

step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) =
  let e2' = step e2
   in Add (Num n1) e2'
step (Add e1 e2) = Add (step e1) e2
-- Implementar step para Times
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) =
  let e2' = step e2
   in Times (Num n1) e2'
step (Times e1 e2) = Times (step e1) e2
step (And BFalse e2) = BFalse
step (And BTrue e2) = e2
step (And e1 e2) = And (step e1) e2
-- Implementar step para Or
step (Or BTrue e2) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2
-- Implementar step para If
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2
-- Paren: evaluate inner, then drop when value
step (Paren e) = if isValue e then e else Paren (step e)
step (App (Lam x tp e1) e2) =
  if (isValue e2)
    then
      subst x e2 e1
    else
      App (Lam x tp e1) (step e2)

eval :: Expr -> Expr
eval e =
  if isValue e
    then
      e
    else
      eval (step e)
