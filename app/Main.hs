module Main where

import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

------------------------------------------------------------
-- TOKENIZER
------------------------------------------------------------

data Token
    = Number Double
    | Plus
    | Minus
    | Times
    | Divide
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []

lexer (x:xs)
    | isSpace x = lexer xs

lexer ('+':xs) = Plus : lexer xs
lexer ('-':xs) = Minus : lexer xs
lexer ('*':xs) = Times : lexer xs
lexer ('/':xs) = Divide : lexer xs

lexer (x:xs)
    | isDigit x =
        let (num, rest) = span (\c -> isDigit c || c == '.') (x:xs)
        in Number (read num) : lexer rest

lexer (x:_) =
    error ("Unknown character: " ++ [x])

------------------------------------------------------------
-- AST
------------------------------------------------------------

data Expr
    = Num Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

------------------------------------------------------------
-- PARSER (very simple, no precedence yet)
------------------------------------------------------------

parse :: [Token] -> Expr
parse tokens =
    build tokens
  where
    build [Number a] = Num a

    build (Number a : Plus : Number b : rest) =
        Add (Num a) (build (Number b : rest))

    build (Number a : Minus : Number b : rest) =
        Sub (Num a) (build (Number b : rest))

    build (Number a : Times : Number b : rest) =
        Mul (Num a) (build (Number b : rest))

    build (Number a : Divide : Number b : rest) =
        Div (Num a) (build (Number b : rest))

    build _ =
        error ("Cannot parse tokens: " ++ show tokens)

------------------------------------------------------------
-- EVALUATOR
------------------------------------------------------------

eval :: Expr -> Double
eval (Num n) = n

eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b

------------------------------------------------------------
-- REPL
------------------------------------------------------------

repl :: IO ()
repl = do
    putStr "integra> "
    input <- getLine

    if input == ":quit"
        then putStrLn "Goodbye."
        else do
            let tokens = lexer input
            let ast = parse tokens
            let result = eval ast

            print result
            repl

main :: IO ()
main = do
    putStrLn "Integra v0.2"
    putStrLn "Basic arithmetic REPL"
    putStrLn "Type :quit to exit"
    repl
