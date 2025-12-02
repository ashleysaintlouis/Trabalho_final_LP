module Main where

import Lexer 
import Parser 
import TypeChecker 
import Interpreter

main :: IO ()
main = do
    input <- getContents   
    let tokens = lexer input
    let ast = parser tokens
    let typedAst = typecheck ast
    print (eval typedAst)
