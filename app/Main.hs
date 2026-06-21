module Main where

import Data.Char (isDigit, isSpace)

data Token
    = Number Double
    | Plus
    | Minus
    | Times
    | Divide
    | LParen  -- Left Paranthesis
    | RParen  -- Right Paranthesis
    deriving (Show)

lexer :: String -> [Token]
lexer [] = []

-- Ignore spaces
lexer (x:xs)
    | isSpace x =
        lexer xs

-- Operators
lexer ('+':xs) =
    Plus : lexer xs

lexer ('-':xs) =
    Minus : lexer xs

lexer ('*':xs) =
    Times : lexer xs

lexer ('/':xs) =
    Divide : lexer xs

lexer ('(':xs) =
    LParen : lexer xs

lexer (')':xs) =
    RParen : lexer xs

-- Numbers
lexer (x:xs)
    | isDigit x =
        let
            digits =
                x : takeWhile (\c -> isDigit c || c == '.') xs

            rest =
                dropWhile (\c -> isDigit c || c == '.') xs
        in
            Number (read digits) : lexer rest

-- Unknown character
lexer (x:_) =
    error ("Unknown character: " ++ [x])

repl :: IO ()
repl = do
    putStr "integra> "
    line <- getLine

    if line == ":quit"
        then putStrLn "Goodbye."
        else do
            print (lexer line)
            repl

main :: IO ()
main = do
    putStrLn "Integra v0.2"
    putStrLn "With Lexer Demo"
    putStrLn "Type :quit to exit"
    repl
