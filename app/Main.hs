{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Complex
import System.Console.Haskeline
    ( Completion, CompletionFunc, InputT
    , completeWord, defaultSettings, getInputLine
    , outputStrLn, runInputT, setComplete, simpleCompletion
    )
import Control.Monad.IO.Class (liftIO)
import System.Process (readProcess)

import System.Exit (exitSuccess)
import Control.Exception (try, catch, SomeException)
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix, tails)

import Integra.Token (lexer)
import Integra.Parser (parse)
import Integra.AST (Expr(..))
import Integra.Evaluator (eval, niceShow, showComplex)
import Integra.Derive (deriv, showExpr)
import Integra.Solver (solveLinear, solveQuadratic, solveCubic, maybeShowRoots)
import Integra.Numerical (adaptSimpson, derivative2, derivativeN, limit, taylorSeries)
import Integra.Graph (generateGraphSVG, generateIntegralSVG, generateMandelbrotSVG, generateJuliaSVG, generateBurningShipSVG)

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
    [ cyan ++ bold ++ "  ─── λ Integra v1.0 ───" ++ reset
    , dim ++ "  " ++ version ++ reset
    , dim ++ "  type " ++ green ++ ":help" ++ dim ++ " for commands, "
              ++ green ++ ":quit" ++ dim ++ " to exit" ++ reset
    ]

version :: String
version = "REPL calculator  ·  complex  ·  graphing  ·  calculus"

------------------------------------------------------------
-- HELP TEXT
------------------------------------------------------------

helpText :: String
helpText = unlines
    [ bold ++ cyan ++ "Integra v1.0 — Commands & Expressions" ++ reset
    , ""
    , bold ++ "Expressions" ++ reset
    , "  " ++ green ++ "Arithmetic" ++ reset ++ "       2 + 3, 4 * 5, 10 / 2, 2^3"
    , "  " ++ green ++ "Implicit mult." ++ reset ++ "    4i, 2x, sin(x)cos(x), 2(x+1)"
    , "  " ++ green ++ "Comparison" ++ reset ++ "        >, >=, <, <=, ==, !=  (returns 0/1)"
    , "  " ++ green ++ "Trig" ++ reset ++ "              sin(x), cos(x), tan(x)"
    , "  " ++ green ++ "Reciprocal trig" ++ reset ++ "    csc(x), sec(x), cot(x)"
    , "  " ++ green ++ "Inverse trig" ++ reset ++ "       asin(x), acos(x), atan(x)"
    , "  " ++ green ++ "Inverse recip." ++ reset ++ "     acsc(x), asec(x), acot(x)"
    , "  " ++ green ++ "Hyperbolic" ++ reset ++ "         sinh(x), cosh(x), tanh(x)"
    , "  " ++ green ++ "Recip. hyp." ++ reset ++ "        csch(x), sech(x), coth(x)"
    , "  " ++ green ++ "Inverse hyp." ++ reset ++ "       asinh(x), acosh(x), atanh(x)"
    , "  " ++ green ++ "Log/Exp/Sqrt" ++ reset ++ "       ln(x), log(x), log2(x), log10(x), exp(x), sqrt(x)"
    , "  " ++ green ++ "Rounding" ++ reset ++ "           floor(x), ceil(x), round(x)"
    , "  " ++ green ++ "Other" ++ reset ++ "              abs(x), sign(x)"
    , "  " ++ green ++ "Complex" ++ reset ++ "            conj(x), re(x), im(x), i"
    , "  " ++ green ++ "Special" ++ reset ++ "            gamma(x), erf(x)"
    , "  " ++ green ++ "Constants" ++ reset ++ "          pi, tau, e, phi, i"
    , "  " ++ green ++ "Variables" ++ reset ++ "          x, " ++ yellow ++ "ans" ++ reset ++ " (last result)"
    , ""
    , bold ++ "Commands" ++ reset
    , "  " ++ green ++ ":help" ++ reset ++ "                          Show this help"
    , "  " ++ green ++ ":about" ++ reset ++ "                         About Integra"
    , "  " ++ green ++ ":clear" ++ reset ++ "                         Clear the screen"
    , "  " ++ green ++ ":quit" ++ reset ++ "                          Exit the REPL"
    , "  " ++ green ++ ":solve" ++ reset ++ " <expr>                  Solve linear   expr = 0 for x"
    , "  " ++ green ++ ":solveq" ++ reset ++ " <expr>                 Solve quadratic expr = 0 for x"
    , "  " ++ green ++ ":solvec" ++ reset ++ " <expr>                 Solve cubic    expr = 0 for x"
    , "  " ++ green ++ ":deriv" ++ reset ++ " <expr> [at <x>]         Symbolic derivative (optionally evaluate)"
    , "  " ++ green ++ ":deriv2" ++ reset ++ " <expr> at <x>          Numerical 2nd derivative"
    , "  " ++ green ++ ":derivn" ++ reset ++ " <expr> order <n> at <x>"
    , "                                  Numerical nth derivative"
    , "  " ++ green ++ ":integral" ++ reset ++ " <expr> [from <a> to <b>]"
    , "                                  Definite integral (graphs shaded area) or antiderivative"
    , "  " ++ green ++ ":limit" ++ reset ++ " <expr> as x -> <a>      Numerical limit"
    , "  " ++ green ++ ":taylor" ++ reset ++ " <expr> at <a> order <n>"
    , "                                  Taylor series at x = a"
    , "  " ++ green ++ ":graph" ++ reset ++ " <expr> [from <a> to <b>]"
    , "                                  SVG function plot"
    , "  " ++ green ++ ":mandelbrot" ++ reset ++ " [w h iter xmin xmax ymin ymax]"
    , "                                  Mandelbrot set SVG (infinite zoom)"
    , "  " ++ green ++ ":julia" ++ reset ++ " <cx> <cy> [w h iter xmin xmax ymin ymax]"
    , "                                  Julia set SVG (infinite zoom)"
    , "  " ++ green ++ ":burningship" ++ reset ++ " [w h iter xmin xmax ymin ymax]"
    , "                                  Burning Ship fractal SVG (infinite zoom)"
    , "  " ++ green ++ ":explain deriv|integral|solve" ++ reset ++ " <expr>"
    , "                                  Step-by-step explanation"
    , ""
    , bold ++ "Precedence" ++ reset
    , "  " ++ green ++ "== != > >= < <=" ++ reset ++ "  (lowest)"
    , "  >  " ++ green ++ "+ -" ++ reset ++ "  >  " ++ green ++ "* /" ++ reset
    , "  >  " ++ green ++ "^" ++ reset ++ "  (right-assoc)  >  " ++ green ++ "implicit *"
    ]

aboutText :: String
aboutText = unlines
    [ bold ++ cyan ++ "Integra v1.0" ++ reset
    , "  A terminal-based REPL calculator with complex numbers,"
    , "  symbolic differentiation, SVG graphing, fractals,"
    , "  calculus, and algebra solving."
    , "  Built with Haskell"
    , ""
    , "  " ++ dim ++ "Author:    aluoty" ++ reset
    , "  " ++ dim ++ "License:   MIT" ++ reset
    , "  " ++ dim ++ "Category:  Math" ++ reset
    ]

------------------------------------------------------------
-- COMPLETION
------------------------------------------------------------

commands :: [String]
commands =
    [ ":about", ":clear", ":help", ":quit"
    , ":solve ", ":solveq ", ":solvec "
    , ":deriv ", ":deriv2 ", ":derivn "
    , ":integral "
    , ":limit "
    , ":taylor "
    , ":graph "
    , ":mandelbrot", ":julia ", ":burningship "
    , ":explain "
    ]

wordCompleter :: String -> IO [Completion]
wordCompleter s = return [simpleCompletion c | c <- commands, c `isPrefixOf` s]

completion :: CompletionFunc IO
completion = completeWord Nothing "" wordCompleter

------------------------------------------------------------
-- COMMAND UTILITIES
------------------------------------------------------------

breakSubstr :: String -> String -> Maybe (String, String)
breakSubstr needle haystack =
    case break (\xs -> isPrefixOf needle xs) (tails haystack) of
        (_, [])   -> Nothing
        (b, a:_) -> Just (take (length b) haystack, drop (length needle) a)

readOrEval :: String -> Complex Double -> Complex Double
readOrEval s ans = case reads s of
    [(n, "")] -> replaceInf (n :+ 0)
    _         -> replaceInf (eval (0 :+ 0) ans (parse (lexer s)))

replaceInf :: Complex Double -> Complex Double
replaceInf z
    | isInfinite (realPart z) = if realPart z > 0 then 1e6 :+ 0 else (-1e6) :+ 0
    | otherwise = z

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------
-- COMMAND PROCESSING
------------------------------------------------------------

processExpr :: String -> Complex Double -> IO (Complex Double)
processExpr s ans = do
    let expr   = parse (lexer s)
        result = eval (0 :+ 0) ans expr
        out = case maybeShowRoots expr of
                    Just roots -> roots
                    Nothing    -> showComplex result
    putStrLn (yellow ++ out ++ reset)
    return result

processCommand :: String -> Complex Double -> IO ()
processCommand s ans
    | Just rest <- stripPrefix ":solve " s      = handleSolve rest ans
    | Just rest <- stripPrefix ":solveq " s     = handleSolveq rest ans
    | Just rest <- stripPrefix ":solvec " s     = handleSolvec rest ans
    | Just rest <- stripPrefix ":deriv2 " s     = handleDeriv2 rest ans
    | Just rest <- stripPrefix ":derivn " s     = handleDerivN rest ans
    | Just rest <- stripPrefix ":deriv " s      = handleDeriv rest ans
    | Just rest <- stripPrefix ":integral " s   = handleIntegral rest ans
    | Just rest <- stripPrefix ":limit " s      = handleLimit rest ans
    | Just rest <- stripPrefix ":taylor " s     = handleTaylor rest ans
    | Just rest <- stripPrefix ":graph " s      = handleGraph rest ans
    | Just rest <- stripPrefix ":mandelbrot" s  = handleMandelbrot (trim rest) ans
    | Just rest <- stripPrefix ":julia " s      = handleJulia rest ans
    | Just rest <- stripPrefix ":burningship" s = handleBurningShip (trim rest) ans
    | Just rest <- stripPrefix ":explain " s    = handleExplain rest ans
    | otherwise = putStrLn (red ++ "Unknown command: " ++ s ++ reset)

------------------------------------------------------------
-- COMMAND HANDLERS
------------------------------------------------------------

-- :deriv <expr> [at <x>]
handleDeriv :: String -> Complex Double -> IO ()
handleDeriv s ans =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                xVal = readOrEval xStr ans
                d = deriv expr
                result = eval xVal ans d
            in putStrLn $ yellow ++ "f'(x) = " ++ showExpr d ++ reset ++ "\n"
                       ++ yellow ++ "f'(" ++ xStr ++ ") = " ++ showComplex result ++ reset
        Nothing ->
            let expr = parse (lexer s)
                d = deriv expr
            in putStrLn $ yellow ++ "f'(x) = " ++ showExpr d ++ reset

-- :deriv2 <expr> at <x>
handleDeriv2 :: String -> Complex Double -> IO ()
handleDeriv2 s ans =
    case breakSubstr " at " s of
        Just (exprStr, xStr) ->
            let expr = parse (lexer exprStr)
                x    = realPart (readOrEval xStr ans)
                r    = derivative2 (\xv -> realPart (eval (xv :+ 0) ans expr)) x
            in putStrLn $ yellow ++ "f''(" ++ xStr ++ ") = " ++ showComplex (r :+ 0) ++ reset
        Nothing -> putStrLn $ red ++ "Usage: :deriv2 <expr> at <x>" ++ reset

-- :derivn <expr> order <n> at <x>
handleDerivN :: String -> Complex Double -> IO ()
handleDerivN s ans =
    case breakSubstr " order " s of
        Just (front, rest) ->
            case breakSubstr " at " rest of
                Just (nStr, xStr) ->
                    let expr = parse (lexer front)
                        n    = round (realPart (readOrEval nStr ans))
                        x    = realPart (readOrEval xStr ans)
                        r    = derivativeN n (\xv -> realPart (eval (xv :+ 0) ans expr)) x
                    in putStrLn $ yellow ++ "f^(" ++ show n ++ ")(" ++ xStr ++ ") = "
                                   ++ showComplex (r :+ 0) ++ reset
                Nothing -> putStrLn $ red ++ "Usage: :derivn <expr> order <n> at <x>" ++ reset
        Nothing -> putStrLn $ red ++ "Usage: :derivn <expr> order <n> at <x>" ++ reset

-- :integral <expr> [from <a> to <b>]
handleIntegral :: String -> Complex Double -> IO ()
handleIntegral s ans =
    case breakSubstr " to " s of
        Just (before, bStr) ->
            case breakSubstr " from " before of
                Just (exprStr, aStr) ->
                    let expr = parse (lexer exprStr)
                        a    = realPart (readOrEval aStr ans)
                        b    = realPart (readOrEval bStr ans)
                        r    = adaptSimpson (\x -> eval (x :+ 0) ans expr) a b
                    in do
                        putStrLn $ yellow ++ "∫ f(x) dx = " ++ showComplex r ++ reset
                        let svg = generateIntegralSVG exprStr a b
                        writeFile "/tmp/integra-integral.svg" svg
                        putStrLn $ green ++ "√ Integral graph saved to /tmp/integra-integral.svg" ++ reset
                        openInBrowser "/tmp/integra-integral.svg"
                Nothing -> putStrLn $ red ++ "Usage: :integral <expr> from <a> to <b>" ++ reset
        Nothing ->
            let expr = parse (lexer s)
                antideriv = findAntiderivative expr
            in putStrLn $ yellow ++ "∫ f(x) dx = " ++ showExpr antideriv ++ " + C" ++ reset

-- :limit <expr> as x -> <a>
handleLimit :: String -> Complex Double -> IO ()
handleLimit s ans =
    case breakSubstr " as x -> " s of
        Just (exprStr, aStr) ->
            let expr = parse (lexer exprStr)
                a    = realPart (readOrEval aStr ans)
                r    = limit (\xv -> realPart (eval (xv :+ 0) ans expr)) a
            in putStrLn $ yellow ++ "lim f(x) as x → " ++ niceShow a ++ " = " ++ niceShow r ++ reset
        Nothing -> putStrLn $ red ++ "Usage: :limit <expr> as x -> <a>" ++ reset

-- :taylor <expr> at <a> order <n>
handleTaylor :: String -> Complex Double -> IO ()
handleTaylor s ans =
    case breakSubstr " at " s of
        Just (front, rest) ->
            case breakSubstr " order " rest of
                Just (aStr, nStr) ->
                    let expr = parse (lexer front)
                        a    = realPart (readOrEval aStr ans)
                        n    = round (realPart (readOrEval nStr ans)) `min` 10
                        r    = taylorSeries (\xv -> realPart (eval (xv :+ 0) ans expr)) a n 0
                    in putStrLn $ yellow ++ "T_" ++ show n ++ "(0) ≈ " ++ niceShow r ++ reset
                Nothing -> putStrLn $ red ++ "Usage: :taylor <expr> at <a> order <n>" ++ reset
        Nothing -> putStrLn $ red ++ "Usage: :taylor <expr> at <a> order <n>" ++ reset

-- :graph <expr> [from <a> to <b>] [yfrom <ya> to <yb>]
handleGraph :: String -> Complex Double -> IO ()
handleGraph s _ =
    let parts = words s
        (exprStr, rest) = break (== "from") parts
        exprStr' = unwords exprStr
        (fromVal, toVal, rest2) = case rest of
            ("from":a:"to":b:rest') -> (Just (read a), Just (read b), rest')
            _ -> (Nothing, Nothing, rest)
        (yMin, yMax) = case rest2 of
            ("yfrom":ya:"to":yb:_) -> (Just (read ya), Just (read yb))
            _ -> (Nothing, Nothing)
    in do
        let svg = generateGraphSVG exprStr' fromVal toVal yMin yMax
        writeFile "/tmp/integra-graph.svg" svg
        putStrLn $ green ++ "√ Graph saved to /tmp/integra-graph.svg" ++ reset
        openInBrowser "/tmp/integra-graph.svg"

-- :mandelbrot [<width> <height> <maxIter> [<xMin> <xMax> <yMin> <yMax>]]
handleMandelbrot :: String -> Complex Double -> IO ()
handleMandelbrot s _ =
    let parts = words s
        width  = if length parts > 0 then read (parts !! 0) else 400
        height = if length parts > 1 then read (parts !! 1) else 400
        maxIter= if length parts > 2 then read (parts !! 2) else 100
        xMin   = if length parts > 3 then read (parts !! 3) else (-2.5)
        xMax   = if length parts > 4 then read (parts !! 4) else 1.0
        yMin   = if length parts > 5 then read (parts !! 5) else (-1.25)
        yMax   = if length parts > 6 then read (parts !! 6) else 1.25
        svg = generateMandelbrotSVG width height maxIter xMin xMax yMin yMax
    in do
        writeFile "/tmp/integra-mandelbrot.svg" svg
        putStrLn $ green ++ "√ Mandelbrot saved to /tmp/integra-mandelbrot.svg" ++ reset
        openInBrowser "/tmp/integra-mandelbrot.svg"

-- :julia <cx> <cy> [<width> <height> <maxIter> [<xMin> <xMax> <yMin> <yMax>]]
handleJulia :: String -> Complex Double -> IO ()
handleJulia s _ =
    let parts = words s
        cx = if length parts > 0 then read (parts !! 0) else (-0.7)
        cy = if length parts > 1 then read (parts !! 1) else 0.27015
        width  = if length parts > 2 then read (parts !! 2) else 400
        height = if length parts > 3 then read (parts !! 3) else 400
        maxIter= if length parts > 4 then read (parts !! 4) else 100
        xMin   = if length parts > 5 then read (parts !! 5) else (-2.0)
        xMax   = if length parts > 6 then read (parts !! 6) else 2.0
        yMin   = if length parts > 7 then read (parts !! 7) else (-1.5)
        yMax   = if length parts > 8 then read (parts !! 8) else 1.5
        svg = generateJuliaSVG cx cy width height maxIter xMin xMax yMin yMax
    in do
        writeFile "/tmp/integra-julia.svg" svg
        putStrLn $ green ++ "√ Julia set saved to /tmp/integra-julia.svg" ++ reset
        openInBrowser "/tmp/integra-julia.svg"

-- :solve <expr>
handleSolve :: String -> Complex Double -> IO ()
handleSolve s _ = do
    let result = solveLinear (parse (lexer s))
    putStrLn $ yellow ++ result ++ reset

-- :solveq <expr>
handleSolveq :: String -> Complex Double -> IO ()
handleSolveq s _ = do
    let result = solveQuadratic (parse (lexer s))
    putStrLn $ yellow ++ result ++ reset

-- :solvec <expr>
handleSolvec :: String -> Complex Double -> IO ()
handleSolvec s _ = do
    let result = solveCubic (parse (lexer s))
    putStrLn $ yellow ++ result ++ reset

-- :burningship [<width> <height> <maxIter> [<xMin> <xMax> <yMin> <yMax>]]
handleBurningShip :: String -> Complex Double -> IO ()
handleBurningShip s _ =
    let parts = words s
        width  = if length parts > 0 then read (parts !! 0) else 400
        height = if length parts > 1 then read (parts !! 1) else 400
        maxIter= if length parts > 2 then read (parts !! 2) else 100
        xMin   = if length parts > 3 then read (parts !! 3) else (-2.5)
        xMax   = if length parts > 4 then read (parts !! 4) else 1.5
        yMin   = if length parts > 5 then read (parts !! 5) else (-2.0)
        yMax   = if length parts > 6 then read (parts !! 6) else 1.0
        svg = generateBurningShipSVG width height maxIter xMin xMax yMin yMax
    in do
        writeFile "/tmp/integra-burningship.svg" svg
        putStrLn $ green ++ "√ Burning Ship saved to /tmp/integra-burningship.svg" ++ reset
        openInBrowser "/tmp/integra-burningship.svg"

-- :explain deriv|integral|solve <expr>
handleExplain :: String -> Complex Double -> IO ()
handleExplain s ans =
    case words s of
        ("deriv":rest) ->
            let exprStr = unwords rest
            in case breakSubstr " at " exprStr of
                Just (eStr, xStr) ->
                    let expr = parse (lexer eStr)
                        xVal = readOrEval xStr ans
                        d = deriv expr
                    in do
                        putStrLn $ cyan ++ bold ++ "Step-by-step differentiation:" ++ reset
                        putStrLn $ "  f(x) = " ++ showExpr expr
                        putStrLn $ "  f'(x) = " ++ showExpr d
                        putStrLn $ "  f'(" ++ xStr ++ ") = " ++ showComplex (eval xVal ans d)
                Nothing ->
                    let expr = parse (lexer exprStr)
                        d = deriv expr
                    in do
                        putStrLn $ cyan ++ bold ++ "Step-by-step differentiation:" ++ reset
                        putStrLn $ "  f(x) = " ++ showExpr expr
                        putStrLn $ "  f'(x) = " ++ showExpr d
        ("integral":rest) ->
            let exprStr = unwords rest
                expr = parse (lexer exprStr)
                antideriv = findAntiderivative expr
            in do
                putStrLn $ cyan ++ bold ++ "Step-by-step integration:" ++ reset
                putStrLn $ "  f(x) = " ++ showExpr expr
                putStrLn $ "  ∫ f(x) dx = " ++ showExpr antideriv ++ " + C"
        ("solve":rest) ->
            let exprStr = unwords rest
                expr = parse (lexer exprStr)
            in do
                putStrLn $ cyan ++ bold ++ "Step-by-step solving:" ++ reset
                putStrLn $ "  Equation: " ++ showExpr expr ++ " = 0"
                putStrLn $ "  Solution: " ++ solveLinear expr
        _ -> putStrLn $ red ++ "Usage: :explain deriv|integral|solve <expr>" ++ reset

------------------------------------------------------------
-- FIND ANTIDERIVATIVE
------------------------------------------------------------

findAntiderivative :: Expr -> Expr
findAntiderivative (Num n)       = Mul (Num n) Var
findAntiderivative (Add a b)     = Add (findAntiderivative a) (findAntiderivative b)
findAntiderivative (Sub a b)     = Sub (findAntiderivative a) (findAntiderivative b)
findAntiderivative (Mul (Num n) (CosE (Mul (Num k) Var)))
    | k /= 0 = Div (Mul (Num n) (SinE (Mul (Num k) Var))) (Num k)
findAntiderivative (Mul (Num n) (SinE (Mul (Num k) Var)))
    | k /= 0 = Div (Mul (Num n) (Sub (Num 0) (CosE (Mul (Num k) Var)))) (Num k)
findAntiderivative (Mul (Num n) (ExpE (Mul (Num k) Var)))
    | k /= 0 = Div (Mul (Num n) (ExpE (Mul (Num k) Var))) (Num k)
findAntiderivative (Pow Var (Num n))
    | n /= -1 = Div (Pow Var (Num (n+1))) (Num (n+1))
findAntiderivative (Div (Num n) Var) = Mul (Num n) (LogE (AbsE Var))
findAntiderivative _                = Mul (Num 0) Var

------------------------------------------------------------
-- BROWSER
------------------------------------------------------------

openInBrowser :: String -> IO ()
openInBrowser path = do
    _ <- tryOpen ["xdg-open", path] `catch` handler
    return ()
  where
    handler :: SomeException -> IO ()
    handler _ = do
        _ <- tryOpen ["open", path] `catch` handler2
        return ()
      where
        handler2 :: SomeException -> IO ()
        handler2 _ = do
            _ <- tryOpen ["start", path] `catch` handler3
            return ()
          where
            handler3 :: SomeException -> IO ()
            handler3 _ = putStrLn $ "Could not open browser. File saved to " ++ path

tryOpen :: [String] -> IO ()
tryOpen (cmd:args) = do
    _ <- readProcess cmd args ""
    return ()
tryOpen [] = return ()

------------------------------------------------------------
-- REPL
------------------------------------------------------------

prompt :: String
prompt = green ++ bold ++ "λ " ++ reset

repl :: Complex Double -> InputT IO ()
repl lastAns = do
    minput <- getInputLine prompt
    case minput of
        Nothing -> liftIO (putStrLn "" >> exitSuccess)
        Just input -> do
            let trimmed = trim input
            if null trimmed then repl lastAns
            else if trimmed `elem` [":quit", ":exit"]
                then outputStrLn $ dim ++ "Goodbye." ++ reset
            else if trimmed == ":help"
                then outputStrLn helpText >> repl lastAns
            else if trimmed == ":about"
                then outputStrLn aboutText >> repl lastAns
            else if trimmed == ":clear"
                then liftIO (putStr "\ESC[2J\ESC[H") >> repl lastAns
            else if ":" `isPrefixOf` trimmed
                then liftIO (safeCommand trimmed lastAns) >>= \case
                    Left err -> outputStrLn (red ++ "Error: " ++ err ++ reset)
                                   >> repl lastAns
                    Right () -> repl lastAns
            else liftIO (safeEval trimmed lastAns) >>= \case
                Left err   -> outputStrLn (red ++ "Error: " ++ err ++ reset)
                                 >> repl lastAns
                Right newAns -> repl newAns

safeEval :: String -> Complex Double -> IO (Either String (Complex Double))
safeEval s ans = do
    r <- try (processExpr s ans) :: IO (Either SomeException (Complex Double))
    return $ case r of
        Left e  -> Left (show e)
        Right a -> Right a

safeCommand :: String -> Complex Double -> IO (Either String ())
safeCommand s ans = do
    r <- try (processCommand s ans) :: IO (Either SomeException ())
    return $ case r of
        Left e  -> Left (show e)
        Right () -> Right ()

main :: IO ()
main = do
    putStr banner
    runInputT (setComplete completion defaultSettings) (repl (0 :+ 0))
