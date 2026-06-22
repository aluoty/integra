module Integra.Graph
    ( generateGraphSVG
    , generateMandelbrotSVG
    , generateJuliaSVG
    ) where

import Data.List (intercalate)

------------------------------------------------------------
-- FUNCTION PLOT
------------------------------------------------------------

generateGraphSVG :: (Double -> Double) -> Double -> Double -> String
generateGraphSVG f xmin xmax = header ++
    "<style>text{font-family:monospace;fill:#888}line{stroke:#333;stroke-width:1}" ++
    "polyline{fill:none;stroke:#0f0;stroke-width:1.5}</style>\n" ++
    axes ++
    "<polyline points=\"" ++ pts ++ "\"/>\n" ++
    "</svg>"
  where
    w = 600; h = 400; margin = 40
    vw = w - 2*margin; vh = h - 2*margin
    xr = xmax - xmin

    xs = [xmin, xmin + xr/200 .. xmax]
    ys = [f x | x <- xs]
    good = filter (\(_,y) -> not (isNaN y || isInfinite y)) (zip xs ys)
    (ymin', ymax') = case good of
        []        -> (-1, 1)
        ((_,y):_) -> go y y good
      where
        go mn mx []           = (mn, mx)
        go mn mx ((_,y):rest) = go (min mn y) (max mx y) rest
    yr = max (ymax' - ymin') 1e-10

    toSvgX x = margin + (x - xmin) / xr * vw
    toSvgY y = margin + vh - (y - ymin') / yr * vh

    pts = intercalate " " [show (toSvgX x) ++ "," ++ show (toSvgY y) | (x,y) <- good]

    axes0 = if xmin <= 0 && xmax >= 0
            then let x0 = toSvgX 0
                 in "<line x1=\"" ++ show x0 ++ "\" y1=\"" ++ show margin ++
                    "\" x2=\"" ++ show x0 ++ "\" y2=\"" ++ show (h - margin) ++ "\" style=\"stroke:#555\"/>\n"
            else ""
    axes1 = if ymin' <= 0 && ymax' >= 0
            then let y0 = toSvgY 0
                 in "<line x1=\"" ++ show margin ++ "\" y1=\"" ++ show y0 ++
                    "\" x2=\"" ++ show (w - margin) ++ "\" y2=\"" ++ show y0 ++ "\" style=\"stroke:#555\"/>\n"
            else ""
    axes = axes0 ++ axes1

    header = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " ++
             show w ++ " " ++ show h ++ "\" style=\"background:#1a1a1a\">\n"

------------------------------------------------------------
-- MANDELBROT SET
------------------------------------------------------------

generateMandelbrotSVG :: Int -> Int -> Int -> String
generateMandelbrotSVG width height maxIter = header ++
    "<style>rect{shape-rendering:crispEdges}</style>\n" ++
    concat [renderRow y | y <- [0..height-1]] ++
    "</svg>"
  where
    w = width; h = height
    header = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " ++
             show w ++ " " ++ show h ++ "\" style=\"background:#000\">\n"

    xmin = -2.5; xmax = 1.0; ymin = -1.25; ymax = 1.25
    xr = xmax - xmin; yr = ymax - ymin

    renderRow y = concat [renderPx x y | x <- [0..w-1]]

    renderPx px py =
        let cx = xmin + fromIntegral px / fromIntegral w * xr
            cy = ymin + fromIntegral py / fromIntegral h * yr
            iter = mandelIter cx cy maxIter
            color = iterColor iter maxIter
        in "<rect x=\"" ++ show px ++ "\" y=\"" ++ show py ++
           "\" width=\"1\" height=\"1\" fill=\"#" ++ color ++ "\"/>"

mandelIter :: Double -> Double -> Int -> Int
mandelIter cx cy maxIt = go 0 0 0
  where
    go ix iy n
        | n >= maxIt = maxIt
        | ix*ix + iy*iy > 4.0 = n
        | otherwise =
            let nx = ix*ix - iy*iy + cx
                ny = 2*ix*iy + cy
            in go nx ny (n+1)

------------------------------------------------------------
-- JULIA SET
------------------------------------------------------------

generateJuliaSVG :: Double -> Double -> Int -> Int -> Int -> String
generateJuliaSVG cx cy width height maxIter = header ++
    "<style>rect{shape-rendering:crispEdges}</style>\n" ++
    concat [renderRow y | y <- [0..height-1]] ++
    "</svg>"
  where
    w = width; h = height
    header = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " ++
             show w ++ " " ++ show h ++ "\" style=\"background:#000\">\n"

    xmin = -1.5; xmax = 1.5; ymin = -1.5; ymax = 1.5
    xr = xmax - xmin; yr = ymax - ymin

    renderRow y = concat [renderPx x y | x <- [0..w-1]]

    renderPx px py =
        let zx = xmin + fromIntegral px / fromIntegral w * xr
            zy = ymin + fromIntegral py / fromIntegral h * yr
            iter = juliaIter zx zy cx cy maxIter
            color = iterColor iter maxIter
        in "<rect x=\"" ++ show px ++ "\" y=\"" ++ show py ++
           "\" width=\"1\" height=\"1\" fill=\"#" ++ color ++ "\"/>"

juliaIter :: Double -> Double -> Double -> Double -> Int -> Int
juliaIter zx0 zy0 cx cy maxIt = go zx0 zy0 0
  where
    go zx zy n
        | n >= maxIt = maxIt
        | zx*zx + zy*zy > 4.0 = n
        | otherwise =
            let nx = zx*zx - zy*zy + cx
                ny = 2*zx*zy + cy
            in go nx ny (n+1)

------------------------------------------------------------
-- COLOR PALETTE
------------------------------------------------------------

iterColor :: Int -> Int -> String
iterColor iter maxIt
    | iter >= maxIt = "000"
    | otherwise =
        let t = fromIntegral iter / fromIntegral maxIt
            r = floor (9 * t) * 28
            g = floor (14.5 * (1 - t)) * 15
            b = floor (9 * (1 - t)) * 28
        in pad (showHex r) ++ pad (showHex g) ++ pad (showHex b)
  where
    pad s = replicate (2 - length s) '0' ++ s
    showHex n = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'] !! (n `mod` 16) : ""
