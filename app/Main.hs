module Main where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (isPrefixOf, tails, stripPrefix)
import Control.Exception (catch, SomeException)

------------------------------------------------------------
-- TOKENIZER
------------------------------------------------------------

data Token
    = NumberTok Double
    | PlusTok
    | MinusTok
    | TimesTok
    | DivideTok
    | PowerTok
    | LParenTok
    | RParenTok
    | VarTok
    | SinTok
    | CosTok
    | TanTok
    | LogTok
    | ExpTok
    | SqrtTok
    | PiTok
    | ETok
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs) | isSpace x = lexer xs
lexer ('+':xs) = PlusTok : lexer xs
lexer ('-':xs) = MinusTok : lexer xs
lexer ('*':xs) = TimesTok : lexer xs
lexer ('/':xs) = DivideTok : lexer xs
lexer ('^':xs) = PowerTok : lexer xs
lexer ('(':xs) = LParenTok : lexer xs
lexer (')':xs) = RParenTok : lexer xs
lexer (x:xs) | isDigit x || x == '.' =
    let (num, rest) = span (\c -> isDigit c || c == '.') (x:xs)
    in NumberTok (read num) : lexer rest
lexer (x:xs) | isAlpha x =
    let (name, rest) = span isAlpha (x:xs)
    in case name of
        "sin"  -> SinTok  : lexer rest
        "cos"  -> CosTok  : lexer rest
        "tan"  -> TanTok  : lexer rest
        "log"  -> LogTok  : lexer rest
        "exp"  -> ExpTok  : lexer rest
        "sqrt" -> SqrtTok : lexer rest
        "pi"   -> PiTok   : lexer rest
        "e"    -> ETok    : lexer rest
        "x"    -> VarTok  : lexer rest
        _      -> error ("Unknown identifier: " ++ name)
lexer (x:_) = error ("Unknown character: " ++ [x])

------------------------------------------------------------
-- AST
------------------------------------------------------------

data Expr
    = Num Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Var
    | SinE Expr
    | CosE Expr
    | TanE Expr
    | LogE Expr
    | ExpE Expr
    | SqrtE Expr
    | Pi
    | E
    deriving (Show)

------------------------------------------------------------
-- PARSER  (precedence: +- < */ < ^ < unary/primary)
------------------------------------------------------------

parse :: [Token] -> Expr
parse tokens = case parseAddSub tokens of
    (e, []) -> e
    (_, rest) -> error ("Unexpected tokens: " ++ show rest)

parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens =
    case parseMulDiv tokens of
        (left, rest) -> go left rest
  where
    go acc (PlusTok  : rest) = let (right, rest') = parseMulDiv rest in go (Add acc right) rest'
    go acc (MinusTok : rest) = let (right, rest') = parseMulDiv rest in go (Sub acc right) rest'
    go acc rest              = (acc, rest)

parseMulDiv :: [Token] -> (Expr, [Token])
parseMulDiv tokens =
    case parsePower tokens of
        (left, rest) -> go left rest
  where
    go acc (TimesTok  : rest) = let (right, rest') = parsePower rest in go (Mul acc right) rest'
    go acc (DivideTok : rest) = let (right, rest') = parsePower rest in go (Div acc right) rest'
    go acc rest               = (acc, rest)

parsePower :: [Token] -> (Expr, [Token])
parsePower tokens =
    let (left, rest) = parseUnary tokens
    in case rest of
        PowerTok : rest' -> let (right, rest'') = parsePower rest' in (Pow left right, rest'')
        _ -> (left, rest)

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (MinusTok : rest) =
    let (e, rest') = parseUnary rest
    in (Sub (Num 0) e, rest')
parseUnary tokens = parsePrimary tokens

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (NumberTok n : rest) = (Num n, rest)
parsePrimary (PiTok    : rest)    = (Pi, rest)
parsePrimary (ETok     : rest)    = (E, rest)
parsePrimary (VarTok   : rest)    = (Var, rest)

parsePrimary (SinTok  : LParenTok : rest) = parseFn rest SinE
parsePrimary (CosTok  : LParenTok : rest) = parseFn rest CosE
parsePrimary (TanTok  : LParenTok : rest) = parseFn rest TanE
parsePrimary (LogTok  : LParenTok : rest) = parseFn rest LogE
parsePrimary (ExpTok  : LParenTok : rest) = parseFn rest ExpE
parsePrimary (SqrtTok : LParenTok : rest) = parseFn rest SqrtE

parsePrimary (LParenTok : rest) =
    let (e, rest') = parseAddSub rest
    in case rest' of
        RParenTok : rest'' -> (e, rest'')
        _ -> error "Expected ')'"

parsePrimary tokens =
    error ("Expected expression, got: " ++ show (take 3 tokens))

parseFn :: [Token] -> (Expr -> Expr) -> (Expr, [Token])
parseFn rest ctor =
    let (e, rest') = parseAddSub rest
    in case rest' of
        RParenTok : rest'' -> (ctor e, rest'')
        _ -> error "Expected ')' after function call"

------------------------------------------------------------
-- EVALUATOR
------------------------------------------------------------

evalWith :: Double -> Expr -> Double
evalWith _ (Num n)   = n
evalWith _ Pi        = pi
evalWith _ E         = exp 1
evalWith x Var       = x
evalWith x (Add a b) = evalWith x a + evalWith x b
evalWith x (Sub a b) = evalWith x a - evalWith x b
evalWith x (Mul a b) = evalWith x a * evalWith x b
evalWith x (Div a b) = evalWith x a / evalWith x b
evalWith x (Pow a b) = evalWith x a ** evalWith x b
evalWith x (SinE a)  = sin (evalWith x a)
evalWith x (CosE a)  = cos (evalWith x a)
evalWith x (TanE a)  = tan (evalWith x a)
evalWith x (LogE a)  = log (evalWith x a)
evalWith x (ExpE a)  = exp (evalWith x a)
evalWith x (SqrtE a) = sqrt (evalWith x a)

------------------------------------------------------------
-- NUMERICAL METHODS
------------------------------------------------------------

derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + h) - f (x - h)) / (2 * h)
  where h = 1e-8

integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = simpson (1000 :: Int)
  where
    simpson n =
        let h  = (b - a) / fromIntegral n
            x i = a + fromIntegral i * h
            s0 = f a + f b
            s1 = sum [f (x i) | i <- [1,3..n-1]]
            s2 = sum [f (x i) | i <- [2,4..n-2]]
        in h / 3 * (s0 + 4 * s1 + 2 * s2)

------------------------------------------------------------
-- ALGEBRA SOLVERS
------------------------------------------------------------

solveLinear :: Expr -> IO ()
solveLinear expr =
    let b = evalWith 0 expr
        a = evalWith 1 expr - b
    in if abs a < 1e-12
        then putStrLn "No unique solution (coefficient of x is 0)"
        else putStrLn $ "x = " ++ show (-b / a)

solveQuadratic :: Expr -> IO ()
solveQuadratic expr =
    let c = evalWith 0 expr
        b = (evalWith 1 expr - evalWith (-1) expr) / 2
        a = evalWith 1 expr - b - c
        d = b * b - 4 * a * c
    in if abs a < 1e-12
        then solveLinear expr
        else if d < 0
            then let re = -b / (2 * a)
                     im = sqrt (-d) / (2 * a)
                 in putStrLn $ "x = " ++ show re ++ " + " ++ show im ++ "i"
                           ++ "\nx = " ++ show re ++ " - " ++ show im ++ "i"
            else let x1 = (-b + sqrt d) / (2 * a)
                     x2 = (-b - sqrt d) / (2 * a)
                 in putStrLn $ "x = " ++ show x1 ++ "\nx = " ++ show x2

------------------------------------------------------------
-- COMMAND HANDLERS
------------------------------------------------------------

breakSubstr :: String -> String -> Maybe (String, String)
breakSubstr needle haystack =
    case break (\xs -> isPrefixOf needle xs) (tails haystack) of
        (_, [])   -> Nothing
        (b, a:_) -> Just (take (length b) haystack, drop (length needle) a)

handleHelp :: IO ()
handleHelp = putStrLn $ unlines
    [ "Integra v0.2 - REPL Calculator"
    , ""
    , "Expressions:"
    , "  Basic arithmetic:   2 + 3, 4 * 5, 10 / 2, 2^3"
    , "  Trig:               sin(x), cos(x), tan(x)"
    , "  Log/Exp/Sqrt:       log(x), exp(x), sqrt(x)"
    , "  Constants:          pi, e"
    , "  Variable:           x"
    , ""
    , "Commands:"
    , "  :help                     Show this help"
    , "  :quit                     Exit the REPL"
    , "  :solve <expr>             Solve linear   <expr> = 0 for x"
    , "  :solveq <expr>            Solve quadratic <expr> = 0 for x"
    , "  :deriv <expr> at <x>      Numerical derivative of <expr> at x"
    , "  :integral <expr> from <a> to <b>"
    , "                            Numerical definite integral from a to b"
    ]

handleSolve :: String -> IO ()
handleSolve s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
    in solveLinear (parse (lexer exprStr))

handleSolveq :: String -> IO ()
handleSolveq s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
    in solveQuadratic (parse (lexer exprStr))

readOrEval :: String -> Double
readOrEval s = case reads s of
    [(n, "")] -> n
    _         -> evalWith 0 (parse (lexer s))

handleDeriv :: String -> IO ()
handleDeriv s =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x     = readOrEval xStr
                r     = derivative (flip evalWith expr) x
            in putStrLn $ "d/dx f(x) at x = " ++ xStr ++ " = " ++ show r
        Nothing ->
            putStrLn "Usage: :deriv <expr> at <x>"

handleIntegral :: String -> IO ()
handleIntegral s =
    case breakSubstr " from " s of
        Just (exprStr, rest) ->
            case breakSubstr " to " rest of
                Just (aStr, bStr) ->
                    let expr = parse (lexer exprStr)
                        a     = readOrEval aStr
                        b     = readOrEval bStr
                        r     = integral (flip evalWith expr) a b
                    in putStrLn $ "Integral from " ++ aStr ++ " to " ++ bStr ++ " = " ++ show r
                Nothing ->
                    putStrLn "Usage: :integral <expr> from <a> to <b>"
        Nothing ->
            putStrLn "Usage: :integral <expr> from <a> to <b>"

------------------------------------------------------------
-- REPL
------------------------------------------------------------

processInput :: String -> IO ()
processInput input
    | Just rest <- stripPrefix ":solve " input     = handleSolve rest
    | Just rest <- stripPrefix ":solveq " input    = handleSolveq rest
    | Just rest <- stripPrefix ":deriv " input     = handleDeriv rest
    | Just rest <- stripPrefix ":integral " input  = handleIntegral rest
    | otherwise = do
        let expr   = parse (lexer input)
            result = evalWith 0 expr
        print result

repl :: IO ()
repl = do
    putStr "integra> "
    input <- getLine
    if input == ":quit"
        then putStrLn "Goodbye."
        else if input == ":help"
            then handleHelp >> repl
            else do
                processInput input
                    `catch` (\e -> putStrLn ("Error: " ++ show (e :: SomeException)))
                repl

main :: IO ()
main = do
    putStrLn "Integra v0.2"
    putStrLn "Type :help for commands, :quit to exit"
    repl
