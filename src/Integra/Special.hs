module Integra.Special (gamma, erf) where

import Data.Complex

-- | Lanczos approximation for the Gamma function on the complex plane
gamma :: Complex Double -> Complex Double
gamma z
    | realPart z < 0.5 = pi / (sin (pi * z) * gamma (1 - z))
    | otherwise = sqrt (2 * pi :+ 0) * (t ** (z - 0.5)) * exp (-t) * series
  where
    t = z + g - 0.5
    g = 7
    p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
         771.32342877765313, -176.61502916214059, 12.507343278686905,
         -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]
    series = p0 + sum [(pi' :+ 0) / (z + (fromIntegral i :+ 0)) | (pi', i) <- zip (drop 1 p) [1..]]
    p0 = head p :+ 0

-- | Error function via Abramowitz & Stegun approximation (real part only)
erf :: Complex Double -> Complex Double
erf z
    | realPart z >= 0 = 1 - t * exp (-(z*z) - (1.26551223 :+ 0)) * poly
    | otherwise = -erf (-z)
  where
    t = 1 / (1 + 0.3275911 * z)
    a1 = 0.254829592; a2 = -0.284496736; a3 = 1.421413741
    a4 = -1.453152027; a5 = 1.061405429
    poly = ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t
