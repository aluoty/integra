{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Complex
import Data.Aeson (object, (.=), Value(..))
import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text as T
import Network.HTTP.Types (status404)
import Control.Exception (try, SomeException, evaluate)
import Data.Char (toLower)

import Integra.Token (Token, lexer)
import Integra.Parser (parse)
import Integra.AST (Expr(..))
import Integra.Evaluator (eval, showComplex)
import Integra.Derive (deriv, showExpr)
import Integra.Solver (solveLinear, solveQuadratic, solveCubic, maybeShowRoots)
import Integra.Numerical (adaptSimpson, derivative2, derivativeN, limit, taylorSeries)
import Integra.Graph (generateGraphSVG, generateIntegralSVG, generateMandelbrotSVG, generateJuliaSVG, generateBurningShipSVG)
import Integra.Antiderivative (findAntiderivative)

main :: IO ()
main = scotty 3000 $ do

    get "/api/eval" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        at   <- param "at"   `rescue` (\(_ :: SomeException) -> return "")
        r <- liftIO $ safeEval (TL.unpack expr) (TL.unpack at)
        json $ object
            [ "result" .= r
            , "expr"   .= expr
            ]

    get "/api/deriv" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        at   <- param "at"   `rescue` (\(_ :: SomeException) -> return "")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object
                [ "deriv" .= ("Error: " <> TL.pack err)
                , "expr"  .= expr
                ]
            Right ast -> do
                let d = deriv ast
                    derivStr = showExpr d
                    resultStr = if not (TL.null at)
                        then case safeParseReal' (TL.unpack at) of
                            Right a -> showComplex (eval (a :+ 0) (0 :+ 0) d)
                            _       -> ""
                        else ""
                json $ object
                    [ "deriv"  .= TL.pack derivStr
                    , "result" .= TL.pack resultStr
                    , "expr"   .= expr
                    , "at"     .= at
                    ]

    get "/api/deriv2" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        at   <- param "at"   `rescue` (\(_ :: SomeException) -> return "")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
            Right ast ->
                case safeParseReal' (TL.unpack at) of
                    Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
                    Right x -> do
                        let r = derivative2 (\xv -> realPart (eval (xv :+ 0) (0 :+ 0) ast)) x
                        json $ object
                            [ "result" .= TL.pack (showComplex (r :+ 0))
                            , "expr"   .= expr
                            , "at"     .= at
                            ]

    get "/api/derivn" $ do
        expr  <- param "expr"  `rescue` (\(_ :: SomeException) -> return "")
        order <- param "order" `rescue` (\(_ :: SomeException) -> return "1")
        at    <- param "at"    `rescue` (\(_ :: SomeException) -> return "")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
            Right ast ->
                case (safeParseReal' (TL.unpack at), safeParseReal' (TL.unpack order)) of
                    (Right x, Right n) -> do
                        let r = derivativeN (round n) (\xv -> realPart (eval (xv :+ 0) (0 :+ 0) ast)) x
                        json $ object
                            [ "result" .= TL.pack (showComplex (r :+ 0))
                            , "expr"   .= expr
                            , "order"  .= order
                            , "at"     .= at
                            ]
                    _ -> json $ object ["result" .= ("Error: invalid at or order" :: TL.Text)]

    get "/api/solve" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        solution <- liftIO $ safeSolve solveLinear (TL.unpack expr)
        json $ object
            [ "solution" .= TL.pack solution
            , "expr"     .= expr
            ]

    get "/api/solveq" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        solution <- liftIO $ safeSolve solveQuadratic (TL.unpack expr)
        json $ object
            [ "solution" .= TL.pack solution
            , "expr"     .= expr
            ]

    get "/api/solvec" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        solution <- liftIO $ safeSolve solveCubic (TL.unpack expr)
        json $ object
            [ "solution" .= TL.pack solution
            , "expr"     .= expr
            ]

    get "/api/integral" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        from <- param "from" `rescue` (\(_ :: SomeException) -> return "0")
        to   <- param "to"   `rescue` (\(_ :: SomeException) -> return "1")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
            Right ast -> do
                let a = realPart (parseBound (lexer (TL.unpack from)))
                    b = realPart (parseBound (lexer (TL.unpack to)))
                    result = adaptSimpson (\x -> eval (x :+ 0) (0 :+ 0) ast) a b
                json $ object
                    [ "result" .= TL.pack (showComplex result)
                    , "expr"   .= expr
                    , "from"   .= from
                    , "to"     .= to
                    ]

    get "/api/antideriv" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
            Right ast -> json $ object
                [ "result" .= TL.pack (showExpr (findAntiderivative ast) ++ " + C")
                , "expr"   .= expr
                ]

    get "/api/graph" $ do
        expr  <- param "expr"  `rescue` (\(_ :: SomeException) -> return "")
        from  <- param "from"  `rescue` (\(_ :: SomeException) -> return "")
        to    <- param "to"    `rescue` (\(_ :: SomeException) -> return "")
        yMin  <- param "yMin"  `rescue` (\(_ :: SomeException) -> return "")
        yMax  <- param "yMax"  `rescue` (\(_ :: SomeException) -> return "")
        let mFrom = if TL.null from then Nothing else Just (r from)
            mTo   = if TL.null to   then Nothing else Just (r to)
            mYMin = if TL.null yMin then Nothing else Just (r yMin)
            mYMax = if TL.null yMax then Nothing else Just (r yMax)
            svg = generateGraphSVG (TL.unpack expr) mFrom mTo mYMin mYMax
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/integral-graph" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        a    <- param "from"  `rescue` (\(_ :: SomeException) -> return "0")
        b    <- param "to"    `rescue` (\(_ :: SomeException) -> return "1")
        let svg = generateIntegralSVG (TL.unpack expr) (r a) (r b)
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/mandelbrot" $ do
        width  <- param "width"  `rescue` (\(_ :: SomeException) -> return "400")
        height <- param "height" `rescue` (\(_ :: SomeException) -> return "400")
        iter   <- param "iter"   `rescue` (\(_ :: SomeException) -> return "100")
        xMin   <- param "xMin"   `rescue` (\(_ :: SomeException) -> return "-2.5")
        xMax   <- param "xMax"   `rescue` (\(_ :: SomeException) -> return "1.0")
        yMin   <- param "yMin"   `rescue` (\(_ :: SomeException) -> return "-1.25")
        yMax   <- param "yMax"   `rescue` (\(_ :: SomeException) -> return "1.25")
        let svg = generateMandelbrotSVG (ri width) (ri height) (ri iter) (r xMin) (r xMax) (r yMin) (r yMax)
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/julia" $ do
        cx     <- param "cx"     `rescue` (\(_ :: SomeException) -> return "-0.7")
        cy     <- param "cy"     `rescue` (\(_ :: SomeException) -> return "0.27015")
        width  <- param "width"  `rescue` (\(_ :: SomeException) -> return "400")
        height <- param "height" `rescue` (\(_ :: SomeException) -> return "400")
        iter   <- param "iter"   `rescue` (\(_ :: SomeException) -> return "100")
        xMin   <- param "xMin"   `rescue` (\(_ :: SomeException) -> return "-2.0")
        xMax   <- param "xMax"   `rescue` (\(_ :: SomeException) -> return "2.0")
        yMin   <- param "yMin"   `rescue` (\(_ :: SomeException) -> return "-1.5")
        yMax   <- param "yMax"   `rescue` (\(_ :: SomeException) -> return "1.5")
        let svg = generateJuliaSVG (r cx) (r cy) (ri width) (ri height) (ri iter) (r xMin) (r xMax) (r yMin) (r yMax)
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/burningship" $ do
        width  <- param "width"  `rescue` (\(_ :: SomeException) -> return "400")
        height <- param "height" `rescue` (\(_ :: SomeException) -> return "400")
        iter   <- param "iter"   `rescue` (\(_ :: SomeException) -> return "100")
        xMin   <- param "xMin"   `rescue` (\(_ :: SomeException) -> return "-2.5")
        xMax   <- param "xMax"   `rescue` (\(_ :: SomeException) -> return "1.5")
        yMin   <- param "yMin"   `rescue` (\(_ :: SomeException) -> return "-2.0")
        yMax   <- param "yMax"   `rescue` (\(_ :: SomeException) -> return "1.0")
        let svg = generateBurningShipSVG (ri width) (ri height) (ri iter) (r xMin) (r xMax) (r yMin) (r yMax)
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/limit" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        a    <- param "as"   `rescue` (\(_ :: SomeException) -> return "0")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
            Right ast ->
                case safeParseReal' (TL.unpack a) of
                    Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
                    Right x -> do
                        let r' = limit (\xv -> realPart (eval (xv :+ 0) (0 :+ 0) ast)) x
                        json $ object
                            [ "result" .= TL.pack (showComplex (r' :+ 0))
                            , "expr"   .= expr
                            , "as"     .= a
                            ]

    get "/api/taylor" $ do
        expr  <- param "expr"  `rescue` (\(_ :: SomeException) -> return "")
        at    <- param "at"    `rescue` (\(_ :: SomeException) -> return "0")
        order <- param "order" `rescue` (\(_ :: SomeException) -> return "5")
        e <- liftIO $ safeParse (TL.unpack expr)
        case e of
            Left err -> json $ object ["result" .= ("Error: " <> TL.pack err)]
            Right ast ->
                case (safeParseReal' (TL.unpack at), safeParseReal' (TL.unpack order)) of
                    (Right a, Right n) -> do
                        let r' = taylorSeries (\xv -> realPart (eval (xv :+ 0) (0 :+ 0) ast)) a (round n `min` 10) 0
                        json $ object
                            [ "result" .= TL.pack (showComplex (r' :+ 0))
                            , "expr"   .= expr
                            , "at"     .= at
                            , "order"  .= order
                            ]
                    _ -> json $ object ["result" .= ("Error: invalid at or order" :: TL.Text)]

    get "/api/explain" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        tp   <- param "type" `rescue` (\(_ :: SomeException) -> return "deriv")
        case tp of
            "deriv" -> do
                e <- liftIO $ safeParse (TL.unpack expr)
                case e of
                    Left err -> json $ object ["text" .= ("Error: " <> TL.pack err)]
                    Right ast -> json $ object
                        [ "text" .= TL.pack ("f(x) = " ++ showExpr ast ++ "\nf'(x) = " ++ showExpr (deriv ast))
                        , "type" .= (tp :: TL.Text)
                        ]
            "integral" -> do
                e <- liftIO $ safeParse (TL.unpack expr)
                case e of
                    Left err -> json $ object ["text" .= ("Error: " <> TL.pack err)]
                    Right ast -> json $ object
                        [ "text" .= TL.pack ("f(x) = " ++ showExpr ast ++ "\n∫ f(x) dx = " ++ showExpr (findAntiderivative ast) ++ " + C")
                        , "type" .= (tp :: TL.Text)
                        ]
            "solve" -> do
                e <- liftIO $ safeParse (TL.unpack expr)
                case e of
                    Left err -> json $ object ["text" .= ("Error: " <> TL.pack err)]
                    Right ast -> json $ object
                        [ "text" .= TL.pack ("Equation: " ++ showExpr ast ++ " = 0\nSolution: " ++ solveLinear ast)
                        , "type" .= (tp :: TL.Text)
                        ]
            _ -> json $ object ["text" .= ("Unknown explain type: " <> tp)]

    get "/api/about" $ do
        json $ object
            [ "text" .= ("Integra v1.0\nA web-based REPL calculator with complex numbers,\nsymbolic differentiation, SVG graphing, fractals,\ncalculus, and algebra solving.\nBuilt with Haskell / scotty\n\nLicense: MIT\nCategory: Math" :: TL.Text)
            ]

    get "/" $ do
        eContent <- liftIO $ try (TLIO.readFile "web/index.html")
        case eContent of
            Right text -> html text
            Left (_ :: SomeException) -> html "<h1>Integra Web</h1><p>web/index.html not found</p>"

    get "/:file" $ do
        file <- param "file"
        let path = "web/" ++ TL.unpack file
            ext  = map toLower (reverse (takeWhile (/= '.') (reverse (TL.unpack file))))
            ctype = case ext of
                "js"   -> "application/javascript"
                "css"  -> "text/css"
                "svg"  -> "image/svg+xml"
                "json" -> "application/json"
                "png"  -> "image/png"
                "ico"  -> "image/x-icon"
                _      -> "text/plain"
        eContent <- liftIO $ try (TLIO.readFile path)
        case eContent of
            Right content -> do
                setHeader "Content-Type" ctype
                text content
            Left (_ :: SomeException) -> do
                status status404
                text "Not found"

r :: TL.Text -> Double
r = read . TL.unpack

ri :: TL.Text -> Int
ri = round . r

safeParse :: String -> IO (Either String Expr)
safeParse s = do
    result <- try (evaluate (parse (lexer s))) :: IO (Either SomeException Expr)
    return $ case result of
        Right expr -> Right expr
        Left e     -> Left (show e)

safeParseReal' :: String -> Either String Double
safeParseReal' s =
    case reads s of
        [(x, "")] -> Right x
        _         -> Left ("Cannot parse number: " ++ s)

safeEval :: String -> String -> IO TL.Text
safeEval exprStr atStr = do
    result <- try (evaluate (parse (lexer exprStr))) :: IO (Either SomeException Expr)
    return $ case result of
        Left e -> TL.pack ("Error: " ++ show e)
        Right ast -> do
            let xVal = case safeParseReal' atStr of
                         Right x -> x :+ 0
                         Left _  -> 0 :+ 0
                evaluated = eval xVal (0 :+ 0) ast
            case maybeShowRoots ast of
                Just roots -> TL.pack roots
                Nothing    -> TL.pack (showComplex evaluated)

safeSolve :: (Expr -> String) -> String -> IO String
safeSolve solver s = do
    result <- try (evaluate (parse (lexer s))) :: IO (Either SomeException Expr)
    return $ case result of
        Right ast -> solver ast
        Left err  -> "Error: " ++ show err

parseBound :: [Token] -> Complex Double
parseBound tokens = replaceInf (eval (0 :+ 0) (0 :+ 0) (parse tokens))

replaceInf :: Complex Double -> Complex Double
replaceInf z
    | isInfinite (realPart z) = if realPart z > 0 then 1e6 :+ 0 else (-1e6) :+ 0
    | otherwise = z
