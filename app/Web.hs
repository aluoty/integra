{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Complex
import Data.Aeson (object, (.=), Value(..))
import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text as T
import Network.HTTP.Types (status404)
import Control.Exception (try, SomeException)

import Integra.Token (Token, lexer)
import Integra.Parser (parse)
import Integra.AST (Expr(..))
import Integra.Evaluator (eval, showComplex)
import Integra.Derive (deriv, showExpr)
import Integra.Solver (solveLinear, solveQuadratic, solveCubic, maybeShowRoots)
import Integra.Numerical (adaptSimpson)
import Integra.Graph (generateGraphSVG, generateIntegralSVG, generateMandelbrotSVG, generateJuliaSVG, generateBurningShipSVG)

main :: IO ()
main = scotty 3000 $ do

    get "/api/eval" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        let tokens = lexer (TL.unpack expr)
            result = tryEval tokens
        json $ object
            [ "result" .= result
            , "expr"   .= expr
            ]

    get "/api/deriv" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        let tokens = lexer (TL.unpack expr)
            ast    = parse tokens
            d      = deriv ast
        json $ object
            [ "deriv"  .= showExpr d
            , "expr"   .= expr
            ]

    get "/api/solve" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        let tokens = lexer (TL.unpack expr)
            solution = solveLinear (parse tokens)
        json $ object
            [ "solution" .= solution
            , "expr"     .= expr
            ]

    get "/api/solveq" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        let tokens = lexer (TL.unpack expr)
            solution = solveQuadratic (parse tokens)
        json $ object
            [ "solution" .= solution
            , "expr"     .= expr
            ]

    get "/api/solvec" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        let tokens = lexer (TL.unpack expr)
            solution = solveCubic (parse tokens)
        json $ object
            [ "solution" .= solution
            , "expr"     .= expr
            ]

    get "/api/integral" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        from <- param "from" `rescue` (\(_ :: SomeException) -> return "0")
        to   <- param "to"   `rescue` (\(_ :: SomeException) -> return "1")
        let e = parse (lexer (TL.unpack expr))
            a = realPart (parseBound (lexer (TL.unpack from)))
            b = realPart (parseBound (lexer (TL.unpack to)))
            result = adaptSimpson (\x -> eval (x :+ 0) (0 :+ 0) e) a b
        json $ object
            [ "result" .= showComplex result
            , "expr"   .= expr
            , "from"   .= from
            , "to"     .= to
            ]

    get "/api/graph" $ do
        expr <- param "expr" `rescue` (\(_ :: SomeException) -> return "")
        from <- param "from" `rescue` (\(_ :: SomeException) -> return "")
        to   <- param "to"   `rescue` (\(_ :: SomeException) -> return "")
        let mFrom = if TL.null from then Nothing else Just (r from)
            mTo   = if TL.null to   then Nothing else Just (r to)
            svg = generateGraphSVG (TL.unpack expr) mFrom mTo Nothing Nothing
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

    get "/" $ do
        eContent <- liftIO $ try (TLIO.readFile "web/index.html")
        case eContent of
            Right text -> html text
            Left (_ :: SomeException) -> html "<h1>Integra Web</h1><p>web/index.html not found</p>"

    get "/:file" $ do
        file <- param "file"
        let path = "web/" ++ TL.unpack file
        eContent <- liftIO $ try (TLIO.readFile path)
        case eContent of
            Right text -> html text
            Left (_ :: SomeException) -> do
                status status404
                text "Not found"

r :: TL.Text -> Double
r = read . TL.unpack

ri :: TL.Text -> Int
ri = round . r

tryEval :: [Token] -> Value
tryEval tokens =
    case parse tokens of
        expr -> case maybeShowRoots expr of
            Just roots -> String (T.pack roots)
            Nothing    -> String (T.pack (showComplex (eval (0 :+ 0) (0 :+ 0) expr)))

parseBound :: [Token] -> Complex Double
parseBound tokens = replaceInf (eval (0 :+ 0) (0 :+ 0) (parse tokens))

replaceInf :: Complex Double -> Complex Double
replaceInf z
    | isInfinite (realPart z) = if realPart z > 0 then 1e6 :+ 0 else (-1e6) :+ 0
    | otherwise = z
