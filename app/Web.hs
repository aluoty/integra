module Main where

import Data.Complex
import Data.Aeson (ToJSON(..), object, (.=), Value(..))
import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL

import Integra.Token (Token, lexer)
import Integra.Parser (parse)
import Integra.AST (Expr(..))
import Integra.Evaluator (eval, showComplex)
import Integra.Derive (deriv, showExpr)
import Integra.Solver (solveLinear, solveQuadratic, maybeShowRoots)
import Integra.Numerical (adaptSimpson)
import Integra.Graph (generateGraphSVG, generateMandelbrotSVG, generateJuliaSVG)

main :: IO ()
main = scotty 3000 $ do

    get "/api/eval" $ do
        expr <- param "expr" `rescue` (\_ -> return "")
        let tokens = lexer (TL.unpack expr)
            result = tryEval tokens
        json $ object
            [ "result" .= result
            , "expr"   .= expr
            ]

    get "/api/deriv" $ do
        expr <- param "expr" `rescue` (\_ -> return "")
        let tokens = lexer (TL.unpack expr)
            ast    = parse tokens
            d      = deriv ast
        json $ object
            [ "deriv"  .= showExpr d
            , "expr"   .= expr
            ]

    get "/api/solve" $ do
        expr <- param "expr" `rescue` (\_ -> return "")
        let tokens = lexer (TL.unpack expr)
            solution = solveLinear (parse tokens)
        json $ object
            [ "solution" .= solution
            , "expr"     .= expr
            ]

    get "/api/solveq" $ do
        expr <- param "expr" `rescue` (\_ -> return "")
        let tokens = lexer (TL.unpack expr)
            solution = solveQuadratic (parse tokens)
        json $ object
            [ "solution" .= solution
            , "expr"     .= expr
            ]

    get "/api/integral" $ do
        expr <- param "expr" `rescue` (\_ -> return "")
        from <- param "from" `rescue` (\_ -> return "0")
        to   <- param "to"   `rescue` (\_ -> return "1")
        let e = parse (lexer (TL.unpack expr))
            a = parseBound (lexer (TL.unpack from))
            b = parseBound (lexer (TL.unpack to))
            result = adaptSimpson (\x -> eval (x :+ 0) (0 :+ 0) e) (realPart a) (realPart b)
        json $ object
            [ "result" .= showComplex result
            , "expr"   .= expr
            , "from"   .= from
            , "to"     .= to
            ]

    get "/api/graph" $ do
        expr <- param "expr" `rescue` (\_ -> return "")
        from <- param "from" `rescue` (\_ -> return "")
        to   <- param "to"   `rescue` (\_ -> return "")
        let mFrom = if TL.null from then Nothing else Just (read (TL.unpack from) :: Double)
            mTo   = if TL.null to   then Nothing else Just (read (TL.unpack to) :: Double)
            svg = generateGraphSVG (TL.unpack expr) mFrom mTo Nothing Nothing
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/mandelbrot" $ do
        width  <- param "width"  `rescue` (\_ -> return "200")
        height <- param "height" `rescue` (\_ -> return "200")
        iter   <- param "iter"   `rescue` (\_ -> return "100")
        let svg = generateMandelbrotSVG (read (TL.unpack width)) (read (TL.unpack height)) (read (TL.unpack iter))
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/api/julia" $ do
        cx     <- param "cx"     `rescue` (\_ -> return "-0.7")
        cy     <- param "cy"     `rescue` (\_ -> return "0.27015")
        width  <- param "width"  `rescue` (\_ -> return "200")
        height <- param "height" `rescue` (\_ -> return "200")
        iter   <- param "iter"   `rescue` (\_ -> return "100")
        let svg = generateJuliaSVG (read (TL.unpack cx)) (read (TL.unpack cy)) (read (TL.unpack width)) (read (TL.unpack height)) (read (TL.unpack iter))
        setHeader "Content-Type" "image/svg+xml"
        text $ TL.pack svg

    get "/" $ do
        html =<< liftIO (BL.readFile "web/index.html")
            `rescue` (\_ -> html "<h1>Integra Web</h1><p>web/index.html not found</p>")

    get "/:file" $ do
        file <- param "file"
        html =<< liftIO (BL.readFile ("web/" ++ TL.unpack file))
            `rescue` (\_ -> do
                status status404
                text "Not found")

tryEval :: [Token] -> Value
tryEval tokens =
    case parse tokens of
        expr -> case maybeShowRoots expr of
            Just roots -> String (TL.pack roots)
            Nothing    -> String (TL.pack (showComplex (eval (0 :+ 0) (0 :+ 0) expr)))

parseBound :: [Token] -> Complex Double
parseBound tokens = replaceInf (eval (0 :+ 0) (0 :+ 0) (parse tokens))

replaceInf :: Complex Double -> Complex Double
replaceInf z
    | isInfinite (realPart z) = if realPart z > 0 then 1e6 :+ 0 else (-1e6) :+ 0
    | otherwise = z
