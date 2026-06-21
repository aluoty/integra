module Main where

import Integra.Token (lexer)
import Integra.Parser (parse)
import Integra.Evaluator (eval, evalWithX)
import Integra.Numerical (derivative, integral)
import Integra.Solver (solveLinear, solveQuadratic)

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix, tails)
import Control.DeepSeq (force)
import Control.Exception (try, evaluate, SomeException)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
    ( Completion
    , CompletionFunc
    , InputT
    , completeWord
    , defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    , setComplete
    , simpleCompletion
    )

------------------------------------------------------------
-- HELP TEXT
------------------------------------------------------------

helpText :: String
helpText = unlines
    [ "Integra v0.3 - REPL Calculator"
    , ""
    , "Expressions:"
    , "  Arithmetic:       2 + 3, 4 * 5, 10 / 2, 2^3"
    , "  Trig:             sin(x), cos(x), tan(x)"
    , "  Inverse trig:     asin(x), acos(x), atan(x)"
    , "  Hyperbolic:       sinh(x), cosh(x), tanh(x)"
    , "  Log/Exp/Sqrt:     log(x), exp(x), sqrt(x)"
    , "  Integer rounding: floor(x), ceil(x), round(x)"
    , "  Absolute value:   abs(x)"
    , "  Constants:        pi, e"
    , "  Variables:        x, ans (last result)"
    , ""
    , "Commands:"
    , "  :help                          Show this help"
    , "  :quit                          Exit the REPL"
    , "  :solve <expr>                  Solve linear   <expr> = 0 for x"
    , "  :solveq <expr>                 Solve quadratic <expr> = 0 for x"
    , "  :deriv <expr> at <x>           Numerical derivative of <expr> at x"
    , "  :integral <expr> from <a> to <b>"
    , "                                 Numerical definite integral from a to b"
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

------------------------------------------------------------
-- COMMAND HANDLERS
------------------------------------------------------------

evalExpr :: String -> Double -> (String, Double)
evalExpr input lastAns =
    let expr  = parse (lexer input)
        result = eval 0 lastAns expr
    in (show result, result)

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
                x     = readOrEval xStr
                r     = derivative (\xv -> evalWithX xv expr) x
            in ("d/dx f(x) at x = " ++ xStr ++ " = " ++ show r, 0)
        Nothing ->
            ("Usage: :deriv <expr> at <x>", 0)

handleIntegral :: String -> (String, Double)
handleIntegral s =
    case breakSubstr " from " s of
        Just (exprStr, rest) ->
            case breakSubstr " to " rest of
                Just (aStr, bStr) ->
                    let expr = parse (lexer exprStr)
                        a     = readOrEval aStr
                        b     = readOrEval bStr
                        r     = integral (\xv -> evalWithX xv expr) a b
                    in ("Integral from " ++ aStr ++ " to " ++ bStr ++ " = " ++ show r, 0)
                Nothing ->
                    ("Usage: :integral <expr> from <a> to <b>", 0)
        Nothing ->
            ("Usage: :integral <expr> from <a> to <b>", 0)

------------------------------------------------------------
-- INPUT PROCESSING
------------------------------------------------------------

processInput :: String -> Double -> (String, Double)
processInput input lastAns
    | Just rest <- stripPrefix ":solve " input     = handleSolve rest
    | Just rest <- stripPrefix ":solveq " input    = handleSolveq rest
    | Just rest <- stripPrefix ":deriv " input     = handleDeriv rest
    | Just rest <- stripPrefix ":integral " input  = handleIntegral rest
    | otherwise                                     = evalExpr input lastAns

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------
-- COMPLETION
------------------------------------------------------------

commands :: [String]
commands =
    [ ":help"
    , ":quit"
    , ":solve "
    , ":solveq "
    , ":deriv "
    , ":integral "
    ]

wordCompleter :: String -> IO [Completion]
wordCompleter s = return [simpleCompletion c | c <- commands, c `isPrefixOf` s]

completion :: CompletionFunc IO
completion = completeWord Nothing "" wordCompleter

------------------------------------------------------------
-- REPL
------------------------------------------------------------

repl :: Double -> InputT IO ()
repl lastAns = do
    minput <- getInputLine ">integra "
    case minput of
        Nothing -> return ()
        Just input -> do
            let trimmed = trim input
            if null trimmed
                then repl lastAns
                else if trimmed == ":quit"
                    then outputStrLn "Goodbye."
                    else if trimmed == ":help"
                        then outputStrLn helpText >> repl lastAns
                        else do
                            result <- safeProcess trimmed lastAns
                            case result of
                                Left err -> do
                                    outputStrLn ("Error: " ++ err)
                                    repl lastAns
                                Right (output, newAns) -> do
                                    outputStrLn output
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
    putStrLn "Integra v0.3"
    putStrLn "Type :help for commands, :quit to exit"
    runInputT (setComplete completion defaultSettings) (repl 0)
