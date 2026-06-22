module Main where

import Integra.Token (lexer)
import Integra.Parser (parse)
import Integra.AST (Expr(..))
import Integra.Evaluator (eval, evalWithX, showComplex, niceShow)
import Integra.Numerical
    ( derivative, derivative2, derivativeN, integral, limit, taylorSeries )
import Integra.Solver (solveLinear, solveQuadratic)
import Integra.Derive (derive, showDeriveSteps, showExpr)
import Integra.Graph (generateGraphSVG, generateMandelbrotSVG, generateJuliaSVG)

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix, tails, intercalate)
import Control.DeepSeq (force)
import Control.Exception (try, evaluate, SomeException)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
    ( Completion, CompletionFunc, InputT
    , completeWord, defaultSettings, getInputLine
    , outputStrLn, runInputT, setComplete, simpleCompletion
    )
import Data.Complex (Complex(..), realPart, magnitude, phase, mkPolar)
import System.Process (system)
import System.Info (os)

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
    [ cyan ++ bold ++ "  ─── λ Integra v0.5 ───" ++ reset
    , dim ++ "  Complex numbers · graphing · calculus · step-by-step" ++ reset
    , dim ++ "  type " ++ green ++ ":help" ++ dim ++ " for commands, "
              ++ green ++ ":quit" ++ dim ++ " to exit" ++ reset
    ]

------------------------------------------------------------
-- HELP TEXT
------------------------------------------------------------

helpText :: String
helpText = unlines
    [ bold ++ cyan ++ "Integra v0.5 — Commands & Expressions" ++ reset
    , ""
    , bold ++ "Expressions" ++ reset
    , "  " ++ green ++ "Arithmetic" ++ reset ++ "     2 + 3, 4 * 5, 10 / 2, 2^3"
    , "  " ++ green ++ "Trig" ++ reset ++ "            sin(x), cos(x), tan(x)"
    , "  " ++ green ++ "Recip. trig" ++ reset ++ "     csc(x), sec(x), cot(x)"
    , "  " ++ green ++ "Inverse trig" ++ reset ++ "    asin(x), acos(x), atan(x)"
    , "  " ++ green ++ "Inv. recip." ++ reset ++ "     acsc(x), asec(x), acot(x)"
    , "  " ++ green ++ "Hyperbolic" ++ reset ++ "      sinh(x), cosh(x), tanh(x)"
    , "  " ++ green ++ "Recip. hyp." ++ reset ++ "     csch(x), sech(x), coth(x)"
    , "  " ++ green ++ "Inv. hyp." ++ reset ++ "       asinh(x), acosh(x), atanh(x)"
    , "  " ++ green ++ "Log/Exp/Sqrt" ++ reset ++ "    log(x), log2(x), log10(x), exp(x), sqrt(x)"
    , "  " ++ green ++ "Complex" ++ reset ++ "         conj(x), re(x), im(x)"
    , "  " ++ green ++ "Rounding" ++ reset ++ "        floor(x), ceil(x), round(x)"
    , "  " ++ green ++ "Other" ++ reset ++ "           abs(x), sign(x)"
    , "  " ++ green ++ "Special" ++ reset ++ "         gamma(x), erf(x)"
    , "  " ++ green ++ "Constants" ++ reset ++ "       pi, tau, e, phi, i"
    , "  " ++ green ++ "Variables" ++ reset ++ "       x, " ++ yellow ++ "ans" ++ reset ++ " (last result)"
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
    , "  " ++ green ++ ":integral" ++ reset ++ " <expr>                  Indefinite integral"
    , "  " ++ green ++ ":limit" ++ reset ++ " <expr> as x -> <a>      Numerical limit"
    , "  " ++ green ++ ":taylor" ++ reset ++ " <expr> at <a> order <n>"
    , "                                  Taylor series at x = a"
    , "  " ++ green ++ ":graph" ++ reset ++ " <expr> from <a> to <b>  Plot function as SVG"
    , "  " ++ green ++ ":mandelbrot" ++ reset ++ "                   Generate Mandelbrot set SVG"
    , "  " ++ green ++ ":julia" ++ reset ++ " <re> <im>              Generate Julia set SVG"
    , "  " ++ green ++ ":explain deriv" ++ reset ++ " <expr>          Show derivative steps"
    , "  " ++ green ++ ":explain deriv" ++ reset ++ " <expr> at <x>   Show derivative steps & evaluate"
    , "  " ++ green ++ ":explain integral" ++ reset ++ " <expr>       Show integral steps"
    , "  " ++ green ++ ":explain solve" ++ reset ++ " <expr>          Show solving steps"
    , ""
    , bold ++ "Precedence" ++ reset
    , "  " ++ green ++ "^" ++ reset ++ "  (right-assoc)  >  " ++ green ++ "* /" ++ reset
    , "  (left-assoc)  >  " ++ green ++ "+ -" ++ reset ++ "  (left-assoc)"
    ]

aboutText :: String
aboutText = unlines
    [ bold ++ cyan ++ "Integra v0.5" ++ reset
    , "  A terminal-based REPL calculator with complex numbers,"
    , "  symbolic differentiation, and SVG graphing."
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

readNumber :: String -> Complex Double
readNumber s = case reads s of
    [(n, "")] -> n :+ 0
    _         -> eval (0 :+ 0) 0 (parse (lexer s))

------------------------------------------------------------
-- NTH ROOT DISPLAY
------------------------------------------------------------

allNthRoots :: Complex Double -> Int -> [Complex Double]
allNthRoots z n
    | magnitude z < 1e-12 = replicate n 0
    | n <= 0 = []
    | otherwise = [mkPolar r ((theta + 2*pi*fromIntegral k) / fromIntegral n) | k <- [0..n-1]]
  where
    r = magnitude z ** (1 / fromIntegral n)
    theta = phase z

maybeShowRoots :: Expr -> Complex Double -> String
maybeShowRoots expr _ =
    case expr of
        Pow base (Div (Num 1) (Num n))
            | n > 1 && abs (n - realToFrac (round n :: Int)) < 1e-12
            , not (hasVar base) ->
                let k = round n
                    baseVal = eval (0 :+ 0) 0 base
                    allRoots = allNthRoots baseVal k
                    shown = map showComplex allRoots
                in "\n" ++ green ++ "All " ++ show k ++ " roots:" ++ reset ++ "\n  " ++
                   intercalate "\n  " shown
        _ -> ""

hasVar :: Expr -> Bool
hasVar Var           = True
hasVar Ans           = True
hasVar (Num _)       = False
hasVar Pi           = False
hasVar E            = False
hasVar Tau          = False
hasVar Phi          = False
hasVar I            = False
hasVar (Add l r)    = hasVar l || hasVar r
hasVar (Sub l r)    = hasVar l || hasVar r
hasVar (Mul l r)    = hasVar l || hasVar r
hasVar (Div l r)    = hasVar l || hasVar r
hasVar (Pow l r)    = hasVar l || hasVar r
hasVar (SinE  e)    = hasVar e
hasVar (CosE  e)    = hasVar e
hasVar (TanE  e)    = hasVar e
hasVar (CscE  e)    = hasVar e
hasVar (SecE  e)    = hasVar e
hasVar (CotE  e)    = hasVar e
hasVar (AsinE e)    = hasVar e
hasVar (AcosE e)    = hasVar e
hasVar (AtanE e)    = hasVar e
hasVar (AcscE e)    = hasVar e
hasVar (AsecE e)    = hasVar e
hasVar (AcotE e)    = hasVar e
hasVar (SinhE e)    = hasVar e
hasVar (CoshE e)    = hasVar e
hasVar (TanhE e)    = hasVar e
hasVar (CschE e)    = hasVar e
hasVar (SechE e)    = hasVar e
hasVar (CothE e)    = hasVar e
hasVar (AsinhE e)   = hasVar e
hasVar (AcoshE e)   = hasVar e
hasVar (AtanhE e)   = hasVar e
hasVar (LogE  e)    = hasVar e
hasVar (Log2E e)    = hasVar e
hasVar (Log10E e)   = hasVar e
hasVar (ExpE  e)    = hasVar e
hasVar (SqrtE e)    = hasVar e
hasVar (AbsE  e)    = hasVar e
hasVar (SignE e)    = hasVar e
hasVar (FloorE e)   = hasVar e
hasVar (CeilE  e)   = hasVar e
hasVar (RoundE e)   = hasVar e
hasVar (GammaE e)   = hasVar e
hasVar (ErfE   e)   = hasVar e
hasVar (ConjE  e)   = hasVar e
hasVar (ReE    e)   = hasVar e
hasVar (ImE    e)   = hasVar e

------------------------------------------------------------
-- PURE COMMAND HANDLERS
------------------------------------------------------------

evalExpr :: String -> Complex Double -> (String, Complex Double, String)
evalExpr input lastAns =
    let expr   = parse (lexer input)
        result = eval (0 :+ 0) lastAns expr
        extra  = maybeShowRoots expr result
    in (showComplex result, result, extra)

handleSolve :: String -> (String, Complex Double)
handleSolve s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
    in (solveLinear (parse (lexer exprStr)), 0 :+ 0)

handleSolveq :: String -> (String, Complex Double)
handleSolveq s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
    in (solveQuadratic (parse (lexer exprStr)), 0 :+ 0)

handleDeriv :: String -> (String, Complex Double)
handleDeriv s =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x    = readNumber xStr
                r    = derivative (\xv -> evalWithX xv expr) (realPart x)
            in ("f'(" ++ showComplex x ++ ") = " ++ showComplex r, 0 :+ 0)
        Nothing -> ("Usage: :deriv <expr> at <x>", 0 :+ 0)

handleDeriv2 :: String -> (String, Complex Double)
handleDeriv2 s =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x    = readNumber xStr
                r    = derivative2 (\xv -> evalWithX xv expr) (realPart x)
            in ("f''(" ++ showComplex x ++ ") = " ++ showComplex r, 0 :+ 0)
        Nothing -> ("Usage: :deriv2 <expr> at <x>", 0 :+ 0)

handleDerivN :: String -> (String, Complex Double)
handleDerivN s =
    case breakSubstr " order " s of
        Just (front, rest) ->
            case breakSubstr " at " rest of
                Just (nStr, xStr) ->
                    let expr = parse (lexer front)
                        n    = round (realPart (readNumber nStr))
                        x    = readNumber xStr
                        r    = derivativeN n (\xv -> evalWithX xv expr) (realPart x)
                    in ("f^(" ++ show n ++ ")(" ++ showComplex x ++ ") = " ++ showComplex r, 0 :+ 0)
                Nothing -> ("Usage: :derivn <expr> order <n> at <x>", 0 :+ 0)
        Nothing -> ("Usage: :derivn <expr> order <n> at <x>", 0 :+ 0)

handleIntegral :: String -> (String, Complex Double)
handleIntegral s =
    case breakSubstr " from " s of
        Just (exprStr, rest) ->
            case breakSubstr " to " rest of
                Just (aStr, bStr) ->
                    let expr = parse (lexer exprStr)
                        a = realPart (readNumber aStr)
                        b = realPart (readNumber bStr)
                    in handleIntegralBounds expr a b aStr bStr
                Nothing -> ("Usage: :integral <expr> from <a> to <b>", 0 :+ 0)
        Nothing -> handleIndefiniteIntegral s

handleIntegralBounds :: Expr -> Double -> Double -> String -> String -> (String, Complex Double)
handleIntegralBounds expr a b aStr bStr =
    let f = \xv -> evalWithX xv expr
        isInf s = s == "inf" || s == "+inf"
        isNegInf s = s == "-inf"
        a' = if isNegInf aStr then -1e6 else if isInf aStr then 1e6 else a
        b' = if isInf bStr then 1e6 else if isNegInf bStr then -1e6 else b
        r = integral f a' b'
        boundsStr = if isNegInf aStr || isInf aStr || isInf bStr || isNegInf bStr
                    then "[" ++ aStr ++ ", " ++ bStr ++ "] ≈ "
                    else "[" ++ niceShow a ++ ", " ++ niceShow b ++ "] = "
    in ("∫ f(x) dx " ++ boundsStr ++ showComplex r, r)

handleIndefiniteIntegral :: String -> (String, Complex Double)
handleIndefiniteIntegral s =
    let expr = parse (lexer s)
    in ("∫ " ++ showExpr expr ++ " dx = " ++ showIndefinite expr, 0 :+ 0)

showIndefinite :: Expr -> String
showIndefinite (Num n)       = niceShow n ++ "x + C"
showIndefinite Var           = "½x² + C"
showIndefinite (Pow Var (Num n))
    | n == -1                = "ln|x| + C"
    | otherwise              = "x^" ++ showNum (n+1) ++ "/" ++ showNum (n+1) ++ " + C"
showIndefinite (SinE Var)    = "-cos(x) + C"
showIndefinite (CosE Var)    = "sin(x) + C"
showIndefinite (ExpE Var)    = "e^x + C"
showIndefinite (Add l r)     = showIndefinite l ++ " + " ++ showIndefinite r
showIndefinite (Sub l r)     = showIndefinite l ++ " - " ++ showIndefinite r
showIndefinite (Mul (Num c) e) = niceShow c ++ "·(" ++ showIndefinite e ++ ")"
showIndefinite _              = "See a calculus reference for the antiderivative"

showNum :: Double -> String
showNum n
    | n == fromIntegral (round n :: Int) = show (round n :: Int)
    | otherwise = let s = show n
                      s' = if '.' `elem` s
                           then reverse (dropWhile (== '0') (reverse s))
                           else s
                  in if last s' == '.' then init s' else s'

handleLimit :: String -> (String, Complex Double)
handleLimit s =
    case breakSubstr " as x -> " s of
        Just (exprStr, aStr) ->
            let expr = parse (lexer exprStr)
                a    = realPart (readNumber aStr)
                r    = limit (\xv -> evalWithX xv expr) a
            in ("lim f(x) as x → " ++ niceShow a ++ " = " ++ showComplex r, 0 :+ 0)
        Nothing -> ("Usage: :limit <expr> as x -> <a>", 0 :+ 0)

handleTaylor :: String -> (String, Complex Double)
handleTaylor s =
    case breakSubstr " at " s of
        Just (front, rest) ->
            case breakSubstr " order " rest of
                Just (aStr, nStr) ->
                    let expr = parse (lexer front)
                        a    = realPart (readNumber aStr)
                        n    = round (realPart (readNumber nStr)) `min` 10
                        r    = taylorSeries (\xv -> evalWithX xv expr) a n 0
                    in ("T_" ++ show n ++ "(0) ≈ " ++ showComplex r, 0 :+ 0)
                Nothing -> ("Usage: :taylor <expr> at <a> order <n>", 0 :+ 0)
        Nothing -> ("Usage: :taylor <expr> at <a> order <n>", 0 :+ 0)

handleExplain :: String -> (String, Complex Double)
handleExplain s
    | Just rest <- stripPrefix "deriv " s    = handleExplainDeriv rest
    | Just rest <- stripPrefix "integral " s = handleExplainIntegral rest
    | Just rest <- stripPrefix "solve " s    = handleExplainSolve rest
    | otherwise = ("Usage: :explain deriv|integral|solve <expr>", 0 :+ 0)

handleExplainDeriv :: String -> (String, Complex Double)
handleExplainDeriv s =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x    = readNumber xStr
                steps = showDeriveSteps expr
                resultE = fst (derive expr)
                resultVal = eval x 0 resultE
            in (steps ++ "\nf'(" ++ showComplex x ++ ") = " ++ showComplex resultVal, 0 :+ 0)
        Nothing ->
            let expr = parse (lexer s)
            in (showDeriveSteps expr, 0 :+ 0)

handleExplainIntegral :: String -> (String, Complex Double)
handleExplainIntegral s =
    let expr = parse (lexer s)
    in ("∫ f(x) dx with f(x) = " ++ showExpr expr ++ "\n\n" ++
        "Using antiderivative rules:\n  " ++ showIndefinite expr, 0 :+ 0)

handleExplainSolve :: String -> (String, Complex Double)
handleExplainSolve s =
    let exprStr = case breakSubstr " = " s of
            Just (left, _) -> left
            Nothing        -> s
        expr = parse (lexer exprStr)
    in ("Solving " ++ showExpr expr ++ " = 0\n\n" ++ solveLinear expr, 0 :+ 0)

------------------------------------------------------------
-- IO COMMAND HANDLERS (Graphing)
------------------------------------------------------------

handleGraph :: String -> IO (String, Complex Double)
handleGraph s =
    case breakSubstr " from " s of
        Just (exprStr, rest) ->
            case breakSubstr " to " rest of
                Just (aStr, bStr) ->
                    let expr = parse (lexer exprStr)
                        a    = realPart (readNumber aStr)
                        b    = realPart (readNumber bStr)
                        f    = \xv -> realPart (evalWithX xv expr)
                        svg  = generateGraphSVG f a b
                        path = "/tmp/integra-graph.svg"
                    in do r <- try (writeFile path svg) :: IO (Either SomeException ())
                          case r of
                              Left e  -> return (red ++ "Error writing SVG: " ++ reset ++ show e, 0 :+ 0)
                              Right _ -> do
                                  openInBrowser path
                                  return (green ++ "✓ Graph saved to " ++ path ++ reset ++
                                         "\n" ++ dim ++ "  Opened in browser" ++ reset, 0 :+ 0)
                Nothing -> return ("Usage: :graph <expr> from <a> to <b>", 0 :+ 0)
        Nothing -> return ("Usage: :graph <expr> from <a> to <b>", 0 :+ 0)

handleMandelbrot :: String -> IO (String, Complex Double)
handleMandelbrot _ =
    let svg  = generateMandelbrotSVG 160 120 100
        path = "/tmp/integra-mandelbrot.svg"
    in do r <- try (writeFile path svg) :: IO (Either SomeException ())
          case r of
              Left e  -> return (red ++ "Error writing Mandelbrot: " ++ reset ++ show e, 0 :+ 0)
              Right _ -> do
                  openInBrowser path
                  return (green ++ "✓ Mandelbrot set saved to " ++ path ++ reset ++
                         "\n" ++ dim ++ "  Opened in browser" ++ reset, 0 :+ 0)

handleJulia :: String -> IO (String, Complex Double)
handleJulia s =
    let parts = words s
    in case parts of
        [reStr, imStr] ->
            let cre = read reStr :: Double
                cim = read imStr :: Double
                svg  = generateJuliaSVG cre cim 160 120 100
                path = "/tmp/integra-julia.svg"
            in do r <- try (writeFile path svg) :: IO (Either SomeException ())
                  case r of
                      Left e  -> return (red ++ "Error writing Julia set: " ++ reset ++ show e, 0 :+ 0)
                      Right _ -> do
                          openInBrowser path
                          return (green ++ "✓ Julia set (c = " ++ show cre ++ " + " ++ show cim ++ "i) saved to " ++ path ++ reset ++
                                 "\n" ++ dim ++ "  Opened in browser" ++ reset, 0 :+ 0)
        _ -> return ("Usage: :julia <re> <im>", 0 :+ 0)

openInBrowser :: FilePath -> IO ()
openInBrowser path = do
    let cmd = case os of
            "darwin"  -> "open " ++ path
            "mingw32" -> "start " ++ path
            _         -> "xdg-open " ++ path
    _ <- system cmd
    return ()

------------------------------------------------------------
-- INPUT PROCESSING
------------------------------------------------------------

processInput :: String -> Complex Double -> IO (String, Complex Double)
processInput input lastAns = do
    let trimmed = trim input
    case () of
        _ | Just rest <- stripPrefix ":graph " trimmed     -> handleGraph rest
          | Just rest <- stripPrefix ":mandelbrot" trimmed  -> handleMandelbrot rest
          | Just rest <- stripPrefix ":julia " trimmed      -> handleJulia rest
          | otherwise                                       -> dispatchPure trimmed lastAns

dispatchPure :: String -> Complex Double -> IO (String, Complex Double)
dispatchPure trimmed lastAns = do
    let result = case () of
            _ | Just rest <- stripPrefix ":solve " trimmed      -> handleSolve rest
              | Just rest <- stripPrefix ":solveq " trimmed     -> handleSolveq rest
              | Just rest <- stripPrefix ":deriv2 " trimmed     -> handleDeriv2 rest
              | Just rest <- stripPrefix ":derivn " trimmed     -> handleDerivN rest
              | Just rest <- stripPrefix ":deriv " trimmed      -> handleDeriv rest
              | Just rest <- stripPrefix ":integral " trimmed   -> handleIntegral rest
              | Just rest <- stripPrefix ":limit " trimmed      -> handleLimit rest
              | Just rest <- stripPrefix ":taylor " trimmed     -> handleTaylor rest
              | Just rest <- stripPrefix ":explain " trimmed    -> handleExplain rest
              | otherwise                                       ->
                    let (s, r, extra) = evalExpr trimmed lastAns
                    in (s ++ extra, r)
    forced <- try (evaluate $ force result) :: IO (Either SomeException (String, Complex Double))
    case forced of
        Left e  -> return (red ++ "Error: " ++ reset ++ show e, lastAns)
        Right v -> return v

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
    , ":graph "
    , ":mandelbrot"
    , ":julia "
    , ":explain deriv ", ":explain integral ", ":explain solve "
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

repl :: Complex Double -> InputT IO ()
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
                                            let isCmdOutput = any (`isPrefixOf` trimmed)
                                                    [":solve", ":solveq", ":explain", ":graph", ":mandelbrot", ":julia"]
                                            if isCmdOutput
                                                then outputStrLn output
                                                else outputStrLn (yellow ++ output ++ reset)
                                            repl newAns

safeProcess :: String -> Complex Double -> InputT IO (Either String (String, Complex Double))
safeProcess input lastAns = liftIO $ do
    r <- try (processInput input lastAns) :: IO (Either SomeException (String, Complex Double))
    return $ case r of
        Left e  -> Left (show e)
        Right v -> Right v

main :: IO ()
main = do
    putStr banner
    runInputT (setComplete completion defaultSettings) (repl (0 :+ 0))
