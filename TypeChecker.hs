module TypeChecker where

import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty

-- ============================================================
-- TYPES
-- ============================================================
typeof ctx BTrue  = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Num n) = Just TNum
typeof ctx (Dbl n) = Just TDbl     
typeof ctx (Str s) = Just TStr     
typeof ctx (Paren e) = typeof ctx e

-- ============================================================
-- ARITHMETIC (Int or Double)
-- ============================================================

typeof ctx (Add e1 e2) = checkMath ctx e1 e2
typeof ctx (Sub e1 e2) = checkMath ctx e1 e2
typeof ctx (Times e1 e2) = checkMath ctx e1 e2

-- ============================================================
-- COMPARISON (>, <, ==)
-- ============================================================
-- 
typeof ctx (Eql e1 e2) = Just TBool 
typeof ctx (Gt e1 e2)  = checkComp ctx e1 e2 
typeof ctx (Lt e1 e2)  = checkComp ctx e1 e2

-- ============================================================
-- BOOLEAN
-- ============================================================
typeof ctx (And e1 e2) = checkBinOp ctx e1 e2 TBool TBool
typeof ctx (Or e1 e2)  = checkBinOp ctx e1 e2 TBool TBool

-- ============================================================
-- CONTROL STRUCTURES (IF)
-- ============================================================
typeof ctx (If e e1 e2) =
    case typeof ctx e of
        Just TBool ->
            case (typeof ctx e1, typeof ctx e2) of
                (Just t1, Just t2) -> Just t1 
                _ -> Nothing
        _ -> Nothing

-- ============================================================
-- VARIABLES & LAMBDA
-- ============================================================
typeof ctx (Var x) = lookup x ctx

typeof ctx (Lam x tp body) =
    let ctx' = (x, tp) : ctx
     in fmap (\tr -> TFun tp tr) (typeof ctx' body)

typeof ctx (App e1 e2) =
    case typeof ctx e1 of
        Just (TFun tp tr) -> Just tr 
        _ -> Nothing

-- ============================================================
-- LISTS (Heterogeneous 
-- ============================================================

typeof ctx (List xs) =
    do
        ts <- mapM (typeof ctx) xs
        Just (TList TAny)

typeof ctx (Head e) = Just TAny 
typeof ctx (Tail e) = Just (TList TAny)
typeof ctx (IsEmpty e) = Just TBool

-- ============================================================
-- HELPER FUNCTIONS
-- ============================================================

checkBinOp ctx e1 e2 tin tout =
    case (typeof ctx e1, typeof ctx e2) of
        (Just t1, Just t2) | t1 == tin && t2 == tin -> Just tout
        _ -> Nothing


checkMath ctx e1 e2 = 
    case (typeof ctx e1, typeof ctx e2) of
        (Just TNum, Just TNum) -> Just TNum
        (Just TDbl, Just TDbl) -> Just TDbl
        (Just TNum, Just TDbl) -> Just TDbl
        (Just TDbl, Just TNum) -> Just TDbl
        (Just TAny, _)         -> Just TAny 
        (_, Just TAny)         -> Just TAny
        _ -> Nothing

-- check
checkComp ctx e1 e2 = Just TBool

-- ============================================================
-- TYPECHECK POINT
-- ============================================================
typecheck :: Expr -> Expr
typecheck e =
    case typeof [] e of
        Just _ -> e
        _ -> error "Type error: Invalid expression or incompatible types!"