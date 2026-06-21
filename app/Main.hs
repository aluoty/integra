module Main where

import Integra.Token (lexer)
import Integra.Parser (parse)
import Integra.Evaluator (eval, evalWithX)
import Integra.Numerical (derivative, derivative2, derivativeN, integral, limit, taylorSeries)
import Integra.Solver (solveLinear, solveQuadratic)

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix, tails)
import Control.DeepSeq (force)
import Control.Exception (try, evaluate, SomeException)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
    ( Completion, CompletionFunc, InputT
    , completeWord, defaultSettings, getInputLine
    , outputStrLn, runInputT, setComplete, simpleCompletion
    )

------------------------------------------------------------
-- ANSI COLORS
------------------------------------------------------------

reset, bold, dim, italic, green, cyan, yellow, red, blue, magenta :: String
reset   = "\ESC[0m"
bold    = "\ESC[1m"
dim     = "\ESC[2m"
italic  = "\ESC[3m"
green   = "\ESC[32m"
cyan    = "\ESC[36m"
yellow  = "\ESC[33m"
red     = "\ESC[31m"
blue    = "\ESC[34m"
magenta = "\ESC[35m"

------------------------------------------------------------
-- BANNER
------------------------------------------------------------

banner :: String
banner = unlines
    [ cyan ++ bold ++ "  ─── λ Integra v0.4 ───" ++ reset
    , dim ++ "  " ++ version ++ reset
    , dim ++ "  type " ++ green ++ ":help" ++ dim ++ " for commands, "
              ++ green ++ ":quit" ++ dim ++ " to exit" ++ reset
    ]

version :: String
version = "REPL calculator  ·  algebra  ·  trig  ·  calculus"

------------------------------------------------------------
-- HELP TEXT
------------------------------------------------------------

helpText :: String
helpText = unlines
    [ bold ++ cyan ++ "Integra v0.4 — Commands & Expressions" ++ reset
    , ""
    , bold ++ "Expressions" ++ reset
    , "  " ++ green ++ "Arithmetic" ++ reset ++ "       2 + 3, 4 * 5, 10 / 2, 2^3"
    , "  " ++ green ++ "Trig" ++ reset ++ "              sin(x), cos(x), tan(x)"
    , "  " ++ green ++ "Reciprocal trig" ++ reset ++ "    csc(x), sec(x), cot(x)"
    , "  " ++ green ++ "Inverse trig" ++ reset ++ "       asin(x), acos(x), atan(x)"
    , "  " ++ green ++ "Inverse recip." ++ reset ++ "     acsc(x), asec(x), acot(x)"
    , "  " ++ green ++ "Hyperbolic" ++ reset ++ "         sinh(x), cosh(x), tanh(x)"
    , "  " ++ green ++ "Recip. hyp." ++ reset ++ "        csch(x), sech(x), coth(x)"
    , "  " ++ green ++ "Log/Exp/Sqrt" ++ reset ++ "       log(x), log2(x), log10(x), exp(x), sqrt(x)"
    , "  " ++ green ++ "Rounding" ++ reset ++ "           floor(x), ceil(x), round(x)"
    , "  " ++ green ++ "Other" ++ reset ++ "              abs(x), sign(x)"
    , "  " ++ green ++ "Special" ++ reset ++ "            gamma(x), erf(x)"
    , "  " ++ green ++ "Constants" ++ reset ++ "          pi, tau, e, phi"
    , "  " ++ green ++ "Variables" ++ reset ++ "          x, " ++ yellow ++ "ans" ++ reset ++ " (last result)"
    , ""
    , bold ++ "Commands" ++ reset
    , "  " ++ green ++ ":help" ++ reset ++ "                          Show this help"
    , "  " ++ green ++ ":about" ++ reset ++ "                         About Integra"
    , "  " ++ green ++ ":quit" ++ reset ++ "                          Exit the REPL"
    , "  " ++ green ++ ":clear" ++ reset ++ "                         Clear the screen"
    , "  " ++ green ++ ":solve" ++ reset ++ " <expr>                  Solve linear   expr = 0 for x"
    , "  " ++ green ++ ":solveq" ++ reset ++ " <expr>                 Solve quadratic expr = 0 for x"
    , "  " ++ green ++ ":deriv" ++ reset ++ " <expr> at <x>           Numerical 1st derivative"
    , "  " ++ green ++ ":deriv2" ++ reset ++ " <expr> at <x>          Numerical 2nd derivative"
    , "  " ++ green ++ ":derivn" ++ reset ++ " <expr> order <n> at <x>"
    , "                                  Numerical nth derivative"
    , "  " ++ green ++ ":integral" ++ reset ++ " <expr> from <a> to <b>"
    , "                                  Numerical definite integral"
    , "  " ++ green ++ ":limit" ++ reset ++ " <expr> as x -> <a>      Numerical limit"
    , "  " ++ green ++ ":taylor" ++ reset ++ " <expr> at <a> order <n>"
    , "                                  Taylor series at x = a"
    , ""
    , bold ++ "Precedence" ++ reset
    , "  " ++ green ++ "^" ++ reset ++ "  (right-assoc)  >  " ++ green ++ "* /" ++ reset ++ "  (left-assoc)  >  " ++ green ++ "+ -" ++ reset ++ "  (left-assoc)"
    ]

aboutText :: String
aboutText = unlines
    [ bold ++ cyan ++ "Integra v0.4" ++ reset
    , "  A terminal-based REPL calculator"
    , "  Built with Haskell"
    , ""
    , "  " ++ dim ++ "Author:    aluoty" ++ reset
    , "  " ++ dim ++ "License:   MIT" ++ reset
    , "  " ++ dim ++ "Category:  Math" ++ reset
    ]

------------------------------------------------------------
-- COMMAND UTILITIES
------------------------------------------------------------

breakSubstr :: String -> String -> Maybe (String, String)
breakSubstr needle haystack =
    case break (\xs -> isPrefixOf needle xs) (tails haystack) of
        (_, [])   -> Nothing
        (b, a:_) -> Just (take (length b) haystack, drop (length needle) a)

readOrEval :: String -> Double
readOrEval s = case reads s of
    [(n, "")] -> n
    _         -> evalWithX 0 (parse (lexer s))

niceShow :: Double -> String
niceShow x
    | isNaN x           = "undefined"
    | isInfinite x      = if x > 0 then "∞" else "-∞"
    | x == 0            = "0"
    | abs x >= 1e12
      || (abs x < 1e-8 && abs x > 0) = show x
    | otherwise =
        let s  = show x
            s' = if '.' `elem` s
                 then reverse (dropWhile (== '0') (reverse s))
                 else s
        in if last s' == '.' then init s' else s'

------------------------------------------------------------
-- COMMAND HANDLERS
------------------------------------------------------------

evalExpr :: String -> Double -> (String, Double)
evalExpr input lastAns =
    let expr   = parse (lexer input)
        result = eval 0 lastAns expr
    in (niceShow result, result)

handleSolve :: String -> (String, Double)
handleSolve s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
    in (solveLinear (parse (lexer exprStr)), 0)

handleSolveq :: String -> (String, Double)
handleSolveq s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
    in (solveQuadratic (parse (lexer exprStr)), 0)

handleDeriv :: String -> (String, Double)
handleDeriv s =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x    = readOrEval xStr
                r    = derivative (\xv -> evalWithX xv expr) x
            in ("f'(" ++ xStr ++ ") = " ++ niceShow r, 0)
        Nothing -> ("Usage: :deriv <expr> at <x>", 0)

handleDeriv2 :: String -> (String, Double)
handleDeriv2 s =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x    = readOrEval xStr
                r    = derivative2 (\xv -> evalWithX xv expr) x
            in ("f''(" ++ xStr ++ ") = " ++ niceShow r, 0)
        Nothing -> ("Usage: :deriv2 <expr> at <x>", 0)

handleDerivN :: String -> (String, Double)
handleDerivN s =
    case breakSubstr " order " s of
        Just (front, rest) ->
            case breakSubstr " at " rest of
                Just (nStr, xStr) ->
                    let expr = parse (lexer front)
                        n    = round (readOrEval nStr)
                        x    = readOrEval xStr
                        r    = derivativeN n (\xv -> evalWithX xv expr) x
                    in ("f^(" ++ show n ++ ")(" ++ xStr ++ ") = " ++ niceShow r, 0)
                Nothing -> ("Usage: :derivn <expr> order <n> at <x>", 0)
        Nothing -> ("Usage: :derivn <expr> order <n> at <x>", 0)

handleIntegral :: String -> (String, Double)
handleIntegral s =
    case breakSubstr " from " s of
        Just (exprStr, rest) ->
            case breakSubstr " to " rest of
                Just (aStr, bStr) ->
                    let expr = parse (lexer exprStr)
                        a    = readOrEval aStr
                        b    = readOrEval bStr
                        r    = integral (\xv -> evalWithX xv expr) a b
                    in ("∫ f(x) dx [" ++ niceShow a ++ ", " ++ niceShow b ++ "] = " ++ niceShow r, 0)
                Nothing -> ("Usage: :integral <expr> from <a> to <b>", 0)
        Nothing -> ("Usage: :integral <expr> from <a> to <b>", 0)

handleLimit :: String -> (String, Double)
handleLimit s =
    case breakSubstr " as x -> " s of
        Just (exprStr, aStr) ->
            let expr = parse (lexer exprStr)
                a    = readOrEval aStr
                r    = limit (\xv -> evalWithX xv expr) a
            in ("lim f(x) as x → " ++ niceShow a ++ " = " ++ niceShow r, 0)
        Nothing -> ("Usage: :limit <expr> as x -> <a>", 0)

handleTaylor :: String -> (String, Double)
handleTaylor s =
    case breakSubstr " at " s of
        Just (front, rest) ->
            case breakSubstr " order " rest of
                Just (aStr, nStr) ->
                    let expr = parse (lexer front)
                        a    = readOrEval aStr
                        n    = round (readOrEval nStr) `min` 10
                        r    = taylorSeries (\xv -> evalWithX xv expr) a n 0
                    in ("T_" ++ show n ++ "(0) ≈ " ++ niceShow r, 0)
                Nothing -> ("Usage: :taylor <expr> at <a> order <n>", 0)
        Nothing -> ("Usage: :taylor <expr> at <a> order <n>", 0)

------------------------------------------------------------
-- INPUT PROCESSING
------------------------------------------------------------

processInput :: String -> Double -> (String, Double)
processInput input lastAns
    | Just rest <- stripPrefix ":solve " input     = handleSolve rest
    | Just rest <- stripPrefix ":solveq " input    = handleSolveq rest
    | Just rest <- stripPrefix ":deriv2 " input    = handleDeriv2 rest
    | Just rest <- stripPrefix ":derivn " input    = handleDerivN rest
    | Just rest <- stripPrefix ":deriv " input     = handleDeriv rest
    | Just rest <- stripPrefix ":integral " input  = handleIntegral rest
    | Just rest <- stripPrefix ":limit " input     = handleLimit rest
    | Just rest <- stripPrefix ":taylor " input    = handleTaylor rest
    | otherwise                                     = evalExpr input lastAns

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------
-- COMPLETION
------------------------------------------------------------

commands :: [String]
commands =
    [ ":about", ":clear", ":help", ":quit"
    , ":solve ", ":solveq "
    , ":deriv ", ":deriv2 ", ":derivn "
    , ":integral "
    , ":limit "
    , ":taylor "
    ]

wordCompleter :: String -> IO [Completion]
wordCompleter s = return [simpleCompletion c | c <- commands, c `isPrefixOf` s]

completion :: CompletionFunc IO
completion = completeWord Nothing "" wordCompleter

------------------------------------------------------------
-- REPL
------------------------------------------------------------

prompt :: String
prompt = green ++ bold ++ "λ> " ++ reset

repl :: Double -> InputT IO ()
repl lastAns = do
    minput <- getInputLine prompt
    case minput of
        Nothing -> return ()
        Just input -> do
            let trimmed = trim input
            if null trimmed
                then repl lastAns
                else if trimmed == ":quit"
                    then outputStrLn $ dim ++ "Goodbye." ++ reset
                    else if trimmed == ":help"
                        then outputStrLn helpText >> repl lastAns
                        else if trimmed == ":about"
                            then outputStrLn aboutText >> repl lastAns
                            else if trimmed == ":clear"
                                then liftIO (putStr "\ESC[2J\ESC[H") >> repl lastAns
                                else do
                                    result <- safeProcess trimmed lastAns
                                    case result of
                                        Left err -> do
                                            outputStrLn (red ++ "Error: " ++ reset ++ err)
                                            repl lastAns
                                        Right (output, newAns) -> do
                                            if ":solve" `isPrefixOf` trimmed
                                                then outputStrLn output
                                                else outputStrLn (yellow ++ output ++ reset)
                                            repl newAns

safeProcess :: String -> Double -> InputT IO (Either String (String, Double))
safeProcess input lastAns = liftIO $ do
    r <- try (evaluate $ force $ processInput input lastAns)
             :: IO (Either SomeException (String, Double))
    return $ case r of
        Left e          -> Left (show e)
        Right (out, ans) -> Right (out, ans)

main :: IO ()
main = do
    putStr banner
    runInputT (setComplete completion defaultSettings) (repl 0)
