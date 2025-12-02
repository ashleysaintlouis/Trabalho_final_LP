{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

-- ============================================================
-- TABELA DE PRECEDÊNCIA 
-- ============================================================
%nonassoc if then else
%right Arrow Lam
%left "||"
%left "&&"
%nonassoc "==" ">" "<"  
%left '+' '-'
%left '*' '/'
%left APP
%left Head Tail IsEmpty

-- ============================================================
-- TOKENS
-- ============================================================
%token
    num         { TokenNum $$ }
    dbl         { TokenDbl $$ }    
    str         { TokenStr $$ }    
    var         { TokenVar $$ }
    true        { TokenTrue }
    false       { TokenFalse }
    
    -- Operadores
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenTimes }
    "=="        { TokenEq }
    ">"         { TokenGt }        
    "<"         { TokenLt }        
    "&&"        { TokenAnd }
    "||"        { TokenOr }
    
    -- Pontuação
    LParen      { TokenLParen }
    RParen      { TokenRParen }
    LBracket    { TokenLBracket }
    RBracket    { TokenRBracket }
    Comma       { TokenComma }
    
    -- Especiais
    Lam         { TokenLam }    
    Colon       { TokenColon }  
    Arrow       { TokenArrow }  
    
    -- Listas
    Head        { TokenHead }
    Tail        { TokenTail }
    IsEmpty     { TokenIsEmpty }
    
    -- Controle
    if          { TokenIf }
    then        { TokenThen }
    else        { TokenElse }
    
    -- Tipos
    TkInt       { TokenInt }
    TkBool      { TokenBool }

%%

-- ============================================================
-- GRAMÁTICA
-- ============================================================

Exp
      : num                     { Num $1 }
      | dbl                     { Dbl $1 }     
      | str                     { Str $1 }     
      | true                    { BTrue }
      | false                   { BFalse }
      | var                     { Var $1 }
      
      -- Operações Matemáticas
      | Exp '+' Exp             { Add $1 $3 }
      | Exp '-' Exp             { Sub $1 $3 }
      | Exp '*' Exp             { Times $1 $3 }
      
      -- Comparadores
      | Exp "==" Exp            { Eql $1 $3 }
      | Exp ">" Exp             { Gt $1 $3 }   
      | Exp "<" Exp             { Lt $1 $3 }   
      
      -- Lógica
      | Exp "&&" Exp            { And $1 $3 }
      | Exp "||" Exp            { Or $1 $3 }
      
      -- Estruturas
      | LParen Exp RParen       { Paren $2 }
      | if Exp then Exp else Exp { If $2 $4 $6 }
      | Lam var Colon Tipo Arrow Exp { Lam $2 $4 $6 }
      | Exp Exp %prec APP       { App $1 $2 }
      
      -- Listas
      | LBracket Items RBracket { List $2 }
      | LBracket RBracket       { List [] }
      | Head Exp                { Head $2 }
      | Tail Exp                { Tail $2 }
      | IsEmpty Exp             { IsEmpty $2 }

Items : Exp                     { [$1] }
      | Exp Comma Items         { $1 : $3 }

Tipo  : TkInt                       { TNum }
      | TkBool                      { TBool }
      | LBracket Tipo RBracket      { TList $2 }    
      | LParen Tipo RParen          { $2 }            
      | Tipo Arrow Tipo             { TFun $1 $3 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Syntax error: " ++ show tokens
}