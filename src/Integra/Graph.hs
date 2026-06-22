module Integra.Graph (generateGraphSVG, generateIntegralSVG, generateMandelbrotSVG, generateJuliaSVG, generateBurningShipSVG) where

import Data.Complex
import Integra.AST (Expr(..))
import Integra.Parser (parse)
import Integra.Token (lexer)
import Integra.Evaluator (eval)

generateGraphSVG :: String -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> String
generateGraphSVG exprStr mFrom mTo mYMin mYMax =
    let expr = parse (lexer exprStr)
        from = maybe (-10) id mFrom
        to   = maybe 10 id mTo
        steps = 400
        h = (to - from) / fromIntegral steps
        points = [ (x, realPart (eval (x :+ 0) (0 :+ 0) expr))
                 | i <- [0..steps], let x = from + h * fromIntegral i ]
        ys = [y | (_, y) <- points, not (isNaN y || isInfinite y)]
        yMin = case mYMin of
                 Just v  -> v
                 Nothing -> if null ys then -1 else minimum ys - (maximum ys - minimum ys) * 0.1
        yMax = case mYMax of
                 Just v  -> v
                 Nothing -> if null ys then 1 else maximum ys + (maximum ys - minimum ys) * 0.1
        width = 600
        height = 400
        mapX x = ((x - from) / (to - from)) * fromIntegral width
        mapY y = fromIntegral height - ((y - yMin) / (yMax - yMin)) * fromIntegral height
        pathData = case filter (\(_,y) -> not (isNaN y || isInfinite y)) points of
                     [] -> ""
                     ((x0,y0):rest) ->
                       let go [] acc = acc
                           go ((x,y):ps) acc = go ps (acc ++ " L " ++ show (mapX x) ++ "," ++ show (mapY y))
                       in "M " ++ show (mapX x0) ++ "," ++ show (mapY y0) ++ go rest ""
        gridLinesX = concatMap (\n ->
            let x = from + (to - from) * (fromIntegral n / 10)
            in if x >= from && x <= to
               then "<line x1='" ++ show (mapX x) ++ "' y1='0' x2='" ++ show (mapX x) ++ "' y2='" ++ show height ++ "' stroke='#ddd' stroke-width='0.5'/>"
               else "") [0..10]
        gridLinesY = concatMap (\n ->
            let y = yMin + (yMax - yMin) * (fromIntegral n / 10)
            in if y >= yMin && y <= yMax
               then "<line x1='0' y1='" ++ show (mapY y) ++ "' x2='" ++ show width ++ "' y2='" ++ show (mapY y) ++ "' stroke='#ddd' stroke-width='0.5'/>"
               else "") [0..10]
        labelsX = concatMap (\n ->
            let x = from + (to - from) * (fromIntegral n / 10)
            in if x >= from && x <= to
               then "<text x='" ++ show (mapX x) ++ "' y='" ++ show (height - 5) ++ "' text-anchor='middle' font-size='10' fill='#666'>" ++ niceNum x ++ "</text>"
               else "") [0..10]
        labelsY = concatMap (\n ->
            let y = yMin + (yMax - yMin) * (fromIntegral n / 10)
            in if y >= yMin && y <= yMax
               then "<text x='5' y='" ++ show (mapY y + 3) ++ "' text-anchor='start' font-size='10' fill='#666'>" ++ niceNum y ++ "</text>"
               else "") [0..10]
    in "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show width ++ "' height='" ++ show height ++ "'>"
        ++ "<rect width='" ++ show width ++ "' height='" ++ show height ++ "' fill='white'/>"
        ++ gridLinesX ++ gridLinesY
        ++ labelsX ++ labelsY
        ++ "<line x1='0' y1='" ++ show (mapY 0) ++ "' x2='" ++ show width ++ "' y2='" ++ show (mapY 0) ++ "' stroke='#999' stroke-width='1'/>"
        ++ "<line x1='" ++ show (mapX 0) ++ "' y1='0' x2='" ++ show (mapX 0) ++ "' y2='" ++ show height ++ "' stroke='#999' stroke-width='1'/>"
        ++ "<path d='" ++ pathData ++ "' fill='none' stroke='#2563eb' stroke-width='2'/>"
        ++ "</svg>"

generateIntegralSVG :: String -> Double -> Double -> String
generateIntegralSVG exprStr a b =
    let expr = parse (lexer exprStr)
        from = a - (b - a) * 0.1
        to   = b + (b - a) * 0.1
        steps = 400
        h = (to - from) / fromIntegral steps
        points = [ (x, realPart (eval (x :+ 0) (0 :+ 0) expr))
                 | i <- [0..steps], let x = from + h * fromIntegral i ]
        ys = [y | (_, y) <- points, not (isNaN y || isInfinite y)]
        yMin = if null ys then -1 else minimum ys - (maximum ys - minimum ys) * 0.1
        yMax = if null ys then 1 else maximum ys + (maximum ys - minimum ys) * 0.1
        width = 600
        height = 400
        mapX x = ((x - from) / (to - from)) * fromIntegral width
        mapY y = fromIntegral height - ((y - yMin) / (yMax - yMin)) * fromIntegral height
        pathData = case filter (\(_,y) -> not (isNaN y || isInfinite y)) points of
                     [] -> ""
                     ((x0,y0):rest) ->
                       let go [] acc = acc
                           go ((x,y):ps) acc = go ps (acc ++ " L " ++ show (mapX x) ++ "," ++ show (mapY y))
                       in "M " ++ show (mapX x0) ++ "," ++ show (mapY y0) ++ go rest ""
        shadeData =
            let shadePoints = filter (\(x',y') -> x' >= a && x' <= b && not (isNaN y') && not (isInfinite y')) points
            in case shadePoints of
                 [] -> ""
                 ((x0,y0):rest) ->
                   let bottomY = mapY 0
                       go [] acc = acc
                       go ((x',y'):ps) acc = go ps (acc ++ " L " ++ show (mapX x') ++ "," ++ show (mapY y'))
                   in "M " ++ show (mapX x0) ++ "," ++ show bottomY
                       ++ " L " ++ show (mapX x0) ++ "," ++ show (mapY y0)
                       ++ go rest ""
                       ++ " L " ++ show (mapX (fst (last shadePoints))) ++ "," ++ show bottomY ++ " Z"
        gridLinesX = concatMap (\n ->
            let x = from + (to - from) * (fromIntegral n / 10)
            in if x >= from && x <= to
               then "<line x1='" ++ show (mapX x) ++ "' y1='0' x2='" ++ show (mapX x) ++ "' y2='" ++ show height ++ "' stroke='#ddd' stroke-width='0.5'/>"
               else "") [0..10]
        gridLinesY = concatMap (\n ->
            let y = yMin + (yMax - yMin) * (fromIntegral n / 10)
            in if y >= yMin && y <= yMax
               then "<line x1='0' y1='" ++ show (mapY y) ++ "' x2='" ++ show width ++ "' y2='" ++ show (mapY y) ++ "' stroke='#ddd' stroke-width='0.5'/>"
               else "") [0..10]
        labelsX = concatMap (\n ->
            let x = from + (to - from) * (fromIntegral n / 10)
            in if x >= from && x <= to
               then "<text x='" ++ show (mapX x) ++ "' y='" ++ show (height - 5) ++ "' text-anchor='middle' font-size='10' fill='#666'>" ++ niceNum x ++ "</text>"
               else "") [0..10]
        labelsY = concatMap (\n ->
            let y = yMin + (yMax - yMin) * (fromIntegral n / 10)
            in if y >= yMin && y <= yMax
               then "<text x='5' y='" ++ show (mapY y + 3) ++ "' text-anchor='start' font-size='10' fill='#666'>" ++ niceNum y ++ "</text>"
               else "") [0..10]
        xAxisY = if yMin <= 0 && yMax >= 0 then mapY 0 else -999
    in "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show width ++ "' height='" ++ show height ++ "'>"
        ++ "<rect width='" ++ show width ++ "' height='" ++ show height ++ "' fill='white'/>"
        ++ gridLinesX ++ gridLinesY
        ++ labelsX ++ labelsY
        ++ (if xAxisY >= 0 then "<line x1='0' y1='" ++ show xAxisY ++ "' x2='" ++ show width ++ "' y2='" ++ show xAxisY ++ "' stroke='#999' stroke-width='1'/>" else "")
        ++ "<line x1='" ++ show (mapX 0) ++ "' y1='0' x2='" ++ show (mapX 0) ++ "' y2='" ++ show height ++ "' stroke='#999' stroke-width='1'/>"
        ++ (if not (null shadeData) then "<path d='" ++ shadeData ++ "' fill='rgba(37,99,235,0.15)' stroke='none'/>" else "")
        ++ "<path d='" ++ pathData ++ "' fill='none' stroke='#2563eb' stroke-width='2'/>"
        ++ "</svg>"

generateMandelbrotSVG :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> String
generateMandelbrotSVG width height maxIter xMin xMax yMin yMax =
    let xStep = (xMax - xMin) / fromIntegral width
        yStep = (yMax - yMin) / fromIntegral height
        svgContent = [renderPixel i j | i <- [0..width-1], j <- [0..height-1]]
        renderPixel i j =
            let cx = xMin + fromIntegral i * xStep
                cy = yMin + fromIntegral j * yStep
                c = cx :+ cy
                iter = mandelbrotIter c maxIter
                color = smoothColor iter maxIter
            in "<rect x='" ++ show i ++ "' y='" ++ show j ++ "' width='1' height='1' fill='" ++ color ++ "'/>"
    in "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show width ++ "' height='" ++ show height ++ "'>"
        ++ concat svgContent
        ++ "</svg>"

generateJuliaSVG :: Double -> Double -> Int -> Int -> Int -> Double -> Double -> Double -> Double -> String
generateJuliaSVG cx cy width height maxIter xMin xMax yMin yMax =
    let xStep = (xMax - xMin) / fromIntegral width
        yStep = (yMax - yMin) / fromIntegral height
        c = cx :+ cy
        svgContent = [renderPixel i j | i <- [0..width-1], j <- [0..height-1]]
        renderPixel i j =
            let zx = xMin + fromIntegral i * xStep
                zy = yMin + fromIntegral j * yStep
                iter = juliaIter (zx :+ zy) c maxIter
                color = smoothColor iter maxIter
            in "<rect x='" ++ show i ++ "' y='" ++ show j ++ "' width='1' height='1' fill='" ++ color ++ "'/>"
    in "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show width ++ "' height='" ++ show height ++ "'>"
        ++ concat svgContent
        ++ "</svg>"

generateBurningShipSVG :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> String
generateBurningShipSVG width height maxIter xMin xMax yMin yMax =
    let xStep = (xMax - xMin) / fromIntegral width
        yStep = (yMax - yMin) / fromIntegral height
        svgContent = [renderPixel i j | i <- [0..width-1], j <- [0..height-1]]
        renderPixel i j =
            let cx = xMin + fromIntegral i * xStep
                cy = yMin + fromIntegral j * yStep
                c = cx :+ cy
                iter = burningShipIter c maxIter
                color = smoothColor iter maxIter
            in "<rect x='" ++ show i ++ "' y='" ++ show j ++ "' width='1' height='1' fill='" ++ color ++ "'/>"
    in "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show width ++ "' height='" ++ show height ++ "'>"
        ++ concat svgContent
        ++ "</svg>"

mandelbrotIter :: Complex Double -> Int -> Double
mandelbrotIter c maxIter = go 0 0
  where
    go n z
        | n >= maxIter = 0
        | magnitude z > 2 = fromIntegral n + 1 - log (log (magnitude z)) / log 2
        | otherwise = go (n+1) (z*z + c)

burningShipIter :: Complex Double -> Int -> Double
burningShipIter c maxIter = go 0 0
  where
    go n z
        | n >= maxIter = 0
        | magnitude z > 2 = fromIntegral n + 1 - log (log (magnitude z)) / log 2
        | otherwise =
            let x = realPart z
                y = imagPart z
                newRe = x*x - y*y + realPart c
                newIm = 2 * abs (x*y) + imagPart c
            in go (n+1) (newRe :+ newIm)

juliaIter :: Complex Double -> Complex Double -> Int -> Double
juliaIter z0 c maxIter = go 0 z0
  where
    go n z
        | n >= maxIter = 0
        | magnitude z > 2 = fromIntegral n + 1 - log (log (magnitude z)) / log 2
        | otherwise = go (n+1) (z*z + c)

smoothColor :: Double -> Int -> String
smoothColor 0 _ = "#000"
smoothColor mu maxIter =
    let t = mu / fromIntegral maxIter
        hue = 360 * 4 * t
        light = 45 + 40 * sin (pi * t)
    in "hsl(" ++ show hue ++ ",100%," ++ show light ++ "%)"

niceNum :: Double -> String
niceNum n
    | abs n < 1e-12 = "0"
    | n == fromIntegral (round n) = show (round n :: Integer)
    | abs n < 0.01 || abs n > 1000 = show n
    | otherwise = show (fromIntegral (round (n * 100)) / 100 :: Double)
