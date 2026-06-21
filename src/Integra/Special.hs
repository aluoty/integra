module Integra.Special (gamma, logGamma, erf, erfc) where

-- | log-gamma using Lanczos approximation (g = 7, 9 coefficients)
logGamma :: Double -> Double
logGamma x
    | x < 0.5 =
        log pi - log (sin (pi * x)) - logGamma (1 - x)
    | otherwise =
        let g = 7.0
            c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
                 771.32342877765313,  -176.61502916214059, 12.507343278686905,
                 -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]
            x' = x - 1
            a  = x' + g + 0.5
            series = case c of
                (c0:rest) -> c0 + sum [ci / (x' + fromIntegral i) | (i, ci) <- zip [1::Int ..] rest]
                []        -> 0
        in 0.5 * log (2 * pi) + (x' + 0.5) * log a - a + log series

-- | Gamma function using Lanczos approximation
gamma :: Double -> Double
gamma x = exp (logGamma x)

-- | Error function via Abramowitz & Stegun approximation
erf :: Double -> Double
erf x
    | abs x > 6 = signum x
    | otherwise =
        let t = 1 / (1 + 0.3275911 * abs x)
            a = [0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429]
            p = t * foldr (\c acc -> c + acc * t) (last a) (init a)
        in signum x * (1 - p * exp (-x * x))

-- | Complementary error function
erfc :: Double -> Double
erfc x = 1 - erf x
