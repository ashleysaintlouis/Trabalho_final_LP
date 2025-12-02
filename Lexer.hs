module Lexer where 

import Data.Char 

-- ============================================================
-- 1. TOKENS
-- ============================================================

data Token 
    = TokenNum Int 
    | TokenDbl Double
    | TokenStr String
    | TokenTrue 
    | TokenFalse
    | TokenPlus 
    | TokenMinus 
    | TokenTimes 
    | TokenEq       
    | TokenGt       
    | TokenLt       
    | TokenAnd 
    | TokenOr 
    | TokenLParen 
    | TokenRParen 
    | TokenLBracket 
    | TokenRBracket 
    | TokenComma 
    | TokenHead 
    | TokenTail 
    | TokenIsEmpty 
    | TokenIf 
    | TokenThen 
    | TokenElse 
    | TokenLam 
    | TokenArrow 
    | TokenColon 
    | TokenInt 
    | TokenBool 
    | TokenVar String 
    deriving (Show, Eq) 

-- ============================================================
-- 2. AST (Expr)
-- ============================================================

data Expr 
    = Num Int 
    | Dbl Double
    | Str String
    | BTrue 
    | BFalse 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Times Expr Expr 
    | Eql Expr Expr 
    | Gt Expr Expr
    | Lt Expr Expr
    | And Expr Expr 
    | Or Expr Expr 
    | Paren Expr 
    | If Expr Expr Expr 
    | Var String 
    | Lam String Ty Expr 
    | App Expr Expr 
    | List [Expr] 
    | Head Expr 
    | Tail Expr 
    | IsEmpty Expr 
    deriving (Show, Eq) 

-- ============================================================
-- 3. TIPOS (Ty)
-- ============================================================

data Ty 
    = TNum 
    | TDbl 
    | TStr 
    | TBool 
    | TFun Ty Ty 
    | TList Ty   
    | TAny       
    deriving (Show, Eq) 

-- ============================================================
-- 4. LEXER
-- ============================================================

lexer :: String -> [Token]
lexer [] = []

-- Símbolos Compostos (ORDEM IMPORTA!)
lexer ('-':'>':cs) = TokenArrow : lexer cs 
lexer ('=':'=':cs) = TokenEq : lexer cs    -- Regra do ==

-- Símbolos Simples
lexer ('>':cs) = TokenGt : lexer cs   
lexer ('<':cs) = TokenLt : lexer cs   
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('-':cs) = TokenMinus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 
lexer ('[':cs) = TokenLBracket : lexer cs 
lexer (']':cs) = TokenRBracket : lexer cs 
lexer (',':cs) = TokenComma : lexer cs 
lexer (':':cs) = TokenColon : lexer cs 
lexer ('\\':cs) = TokenLam : lexer cs 
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs 

-- Strings
lexer ('"':cs) = lexString cs

-- Espaços, Números e Palavras
lexer (c:cs) 
    | isSpace c = lexer cs 
    | isDigit c = lexNum (c:cs) 
    | isAlpha c = lexKw (c:cs) 
    | otherwise = error ("Lexical error: caractere desconhecido " ++ [c]) 

-- ============================================================
-- 5. FUNÇÕES AUXILIARES
-- ============================================================

lexString cs =
    let (str, rest) = span (/= '"') cs
    in if null rest 
       then error "Lexical error: string sem fim"
       else TokenStr str : lexer (drop 1 rest) 

lexNum cs = 
    let (digits, rest) = span isDigit cs
    in case rest of
        ('.':r) -> 
            let (frac, rest2) = span isDigit r
            in if null frac
               then error "Lexical error: float invalido"
               else TokenDbl (read (digits ++ "." ++ frac)) : lexer rest2
        _ -> 
            TokenNum (read digits) : lexer rest

lexKw s = 
    let (w, rest) = span isAlpha s 
    in case w of 
        "true"    -> TokenTrue : lexer rest 
        "false"   -> TokenFalse : lexer rest 
        "if"      -> TokenIf : lexer rest 
        "then"    -> TokenThen : lexer rest 
        "else"    -> TokenElse : lexer rest 
        "head"    -> TokenHead : lexer rest 
        "tail"    -> TokenTail : lexer rest 
        "isEmpty" -> TokenIsEmpty : lexer rest 
        "isempty" -> TokenIsEmpty : lexer rest 
        "Int"     -> TokenInt : lexer rest 
        "Num"     -> TokenInt : lexer rest 
        "Bool"    -> TokenBool : lexer rest 
        _         -> TokenVar w : lexer rest