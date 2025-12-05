module Interpreter where

import Lexer
import Parser

-- ============================================================
-- VERIFICAÇÃO DE VALOR
-- ============================================================
isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
<<<<<<< HEAD
isValue (Dbl _) = True       
isValue (Str _) = True       

isValue (Lam _ _ _) = True
isValue (List []) = True
isValue (List (x:xs)) = isValue x && isValue (List xs)
isValue _ = False

-- ============================================================
-- SUBSTITUIÇÃO
-- ============================================================
subst :: String -> Expr -> Expr -> Expr
subst x s y@(Var v) = if x == v then s else y
subst x s (Num n) = Num n
<<<<<<< HEAD
subst x s (Dbl n) = Dbl n    
subst x s (Str n) = Str n    


subst x s BTrue = BTrue
subst x s BFalse = BFalse
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2)
subst x s (Sub t1 t2) = Sub (subst x s t1) (subst x s t2)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2)
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (Eql t1 t2) = Eql (subst x s t1) (subst x s t2)
<<<<<<< HEAD
subst x s (Gt t1 t2)  = Gt (subst x s t1) (subst x s t2) 
subst x s (Lt t1 t2)  = Lt (subst x s t1) (subst x s t2) 
-- =======
subst x s (Paren e) = Paren (subst x s e)
subst x s (If e1 e2 e3) = If (subst x s e1) (subst x s e2) (subst x s e3)
subst x s lam@(Lam y tp t1) = if x == y then lam else Lam y tp (subst x s t1)
subst x s (List list) = List (map (subst x s) list)
subst x s (Head e)    = Head (subst x s e)
subst x s (Tail e)    = Tail (subst x s e)
subst x s (IsEmpty e) = IsEmpty (subst x s e)

-- ============================================================
-- AVALIAÇÃO (STEP)
-- ============================================================
step :: Expr -> Expr

-- SOMA (Suporta Int e Double)
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Dbl n1) (Dbl n2)) = Dbl (n1 + n2)
step (Add (Num n1) (Dbl n2)) = Dbl (fromIntegral n1 + n2)
step (Add (Dbl n1) (Num n2)) = Dbl (n1 + fromIntegral n2)
step (Add v1 v2) | isValue v1 = Add v1 (step v2) | otherwise = Add (step v1) v2

-- SUBTRAÇÃO
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Dbl n1) (Dbl n2)) = Dbl (n1 - n2)
step (Sub (Num n1) (Dbl n2)) = Dbl (fromIntegral n1 - n2)
step (Sub (Dbl n1) (Num n2)) = Dbl (n1 - fromIntegral n2)
step (Sub v1 v2) | isValue v1 = Sub v1 (step v2) | otherwise = Sub (step v1) v2

-- MULTIPLICAÇÃO
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Dbl n1) (Dbl n2)) = Dbl (n1 * n2)
step (Times (Num n1) (Dbl n2)) = Dbl (fromIntegral n1 * n2)
step (Times (Dbl n1) (Num n2)) = Dbl (n1 * fromIntegral n2)
step (Times v1 v2) | isValue v1 = Times v1 (step v2) | otherwise = Times (step v1) v2

-- IGUALDADE (==)
step (Eql (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse
step (Eql (Dbl n1) (Dbl n2)) = if n1 == n2 then BTrue else BFalse
step (Eql (Str s1) (Str s2)) = if s1 == s2 then BTrue else BFalse
step (Eql BTrue BTrue) = BTrue
step (Eql BFalse BFalse) = BTrue
step (Eql BTrue BFalse) = BFalse
step (Eql BFalse BTrue) = BFalse
step (Eql v1 v2) | isValue v1 = Eql v1 (step v2) | otherwise = Eql (step v1) v2

-- MAIOR QUE (>)
step (Gt (Num n1) (Num n2)) = if n1 > n2 then BTrue else BFalse
step (Gt (Dbl n1) (Dbl n2)) = if n1 > n2 then BTrue else BFalse
step (Gt (Num n1) (Dbl n2)) = if fromIntegral n1 > n2 then BTrue else BFalse
step (Gt (Dbl n1) (Num n2)) = if n1 > fromIntegral n2 then BTrue else BFalse
step (Gt v1 v2) | isValue v1 = Gt v1 (step v2) | otherwise = Gt (step v1) v2

-- MENOR QUE (<)
step (Lt (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (Lt (Dbl n1) (Dbl n2)) = if n1 < n2 then BTrue else BFalse
step (Lt (Num n1) (Dbl n2)) = if fromIntegral n1 < n2 then BTrue else BFalse
step (Lt (Dbl n1) (Num n2)) = if n1 < fromIntegral n2 then BTrue else BFalse
step (Lt v1 v2) | isValue v1 = Lt v1 (step v2) | otherwise = Lt (step v1) v2

-- LÓGICA
step (And BTrue e2) = e2
step (And BFalse _) = BFalse
step (And e1 e2) = And (step e1) e2

step (Or BTrue _) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2

-- ESTRUTURAS
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2
step (Paren e) = if isValue e then e else Paren (step e)

-- APLICAÇÃO
step (App (Lam x tp e1) e2) =
  if isValue e2 then subst x e2 e1 else App (Lam x tp e1) (step e2)
step (App e1 e2) = App (step e1) e2

-- LISTAS
step (Head (List (x:xs))) | isValue x = x
step (Head (List [])) = error "Runtime Error: Head of empty list"
step (Head e) = Head (step e)

step (Tail (List (x:xs))) | isValue (List (x:xs)) = List xs
step (Tail (List [])) = error "Runtime Error: Tail of empty list"
step (Tail e) = Tail (step e)

step (IsEmpty (List [])) = BTrue
step (IsEmpty (List _))  = BFalse
step (IsEmpty e) = IsEmpty (step e)

-- Avaliação interna da lista
step (List list) = List (stepListElement list)
  where
    stepListElement [] = error "List stuck"
    stepListElement (x:xs)
        | isValue x = x : stepListElement xs
        | otherwise = step x : xs

step e = error ("Stuck or Runtime Error: " ++ show e)

eval :: Expr -> Expr
eval e = if isValue e then e else eval (step e)