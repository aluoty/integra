module Integra.Evaluator (eval, evalWithX, showComplex, niceShow) where

import Integra.AST (Expr(..))
import Integra.Special (gamma, erf)
import Data.Complex (Complex(..), realPart, imagPart, magnitude)

eval :: Complex Double -> Complex Double -> Expr -> Complex Double
eval x ansVal = go
  where
    go (Num n)       = n :+ 0
    go Pi            = pi :+ 0
    go E             = exp 1 :+ 0
    go Tau           = (2 * pi) :+ 0
    go Phi           = ((1 + sqrt 5) / 2) :+ 0
    go I             = 0 :+ 1
    go Var           = x
    go Ans           = ansVal
    go (Add l r)     = go l + go r
    go (Sub l r)     = go l - go r
    go (Mul l r)     = go l * go r
    go (Div l r)     = go l / go r
    go (Pow l r)     = go l ** go r
    go (SinE   e)    = sin (go e)
    go (CosE   e)    = cos (go e)
    go (TanE   e)    = tan (go e)
    go (CscE   e)    = 1 / sin (go e)
    go (SecE   e)    = 1 / cos (go e)
    go (CotE   e)    = 1 / tan (go e)
    go (AsinE  e)    = asin (go e)
    go (AcosE  e)    = acos (go e)
    go (AtanE  e)    = atan (go e)
    go (AcscE  e)    = asin (1 / go e)
    go (AsecE  e)    = acos (1 / go e)
    go (AcotE  e)    = atan (1 / go e)
    go (SinhE  e)    = sinh (go e)
    go (CoshE  e)    = cosh (go e)
    go (TanhE  e)    = tanh (go e)
    go (CschE  e)    = 1 / sinh (go e)
    go (SechE  e)    = 1 / cosh (go e)
    go (CothE  e)    = 1 / tanh (go e)
    go (AsinhE e)    = asinh (go e)
    go (AcoshE e)    = acosh (go e)
    go (AtanhE e)    = atanh (go e)
    go (LogE   e)    = log (go e)
    go (Log2E  e)    = log (go e) / log (2 :+ 0)
    go (Log10E e)    = log (go e) / log (10 :+ 0)
    go (ExpE   e)    = exp (go e)
    go (SqrtE  e)    = sqrt (go e)
    go (AbsE   e)    = magnitude (go e) :+ 0
    go (SignE  e)    = signum (go e)
    go (FloorE e)    = (fromIntegral (floor (asReal (go e)) :: Int) :+ 0)
    go (CeilE  e)    = (fromIntegral (ceiling (asReal (go e)) :: Int) :+ 0)
    go (RoundE e)    = (fromIntegral (round (asReal (go e)) :: Int) :+ 0)
    go (GammaE e)    = gamma (asReal (go e)) :+ 0
    go (ErfE   e)    = erf (asReal (go e)) :+ 0
    go (ConjE  e)    = let z = go e in realPart z :+ (-imagPart z)
    go (ReE    e)    = realPart (go e) :+ 0
    go (ImE    e)    = imagPart (go e) :+ 0

    asReal z
        | abs (imagPart z) < 1e-12 = realPart z
        | otherwise = error "Expected a real number, got complex"

evalWithX :: Double -> Expr -> Complex Double
evalWithX x = eval (x :+ 0) 0

------------------------------------------------------------
-- DISPLAY HELPERS
------------------------------------------------------------

niceShow :: Double -> String
niceShow x
    | isNaN x           = "undefined"
    | isInfinite x      = if x > 0 then "∞" else "-∞"
    | x == 0            = "0"
    | abs x >= 1e12
      || (abs x < 1e-8 && abs x > 0) = show x
    | otherwise =
        let s  = show x
            s' = if '.' `elem` s
                 then reverse (dropWhile (== '0') (reverse s))
                 else s
        in if last s' == '.' then init s' else s'

showComplex :: Complex Double -> String
showComplex z
    | isNaN (realPart z) || isInfinite (realPart z)
        = niceShow (realPart z)
    | isNaN (imagPart z) || isInfinite (imagPart z)
        = niceShow (imagPart z) ++ "i"
    | abs (imagPart z) < 1e-12
        = niceShow (realPart z)
    | abs (realPart z) < 1e-12
        = showImag (imagPart z)
    | imagPart z > 0
        = niceShow (realPart z) ++ " + " ++ showImag (imagPart z)
    | otherwise
        = niceShow (realPart z) ++ " - " ++ showImag (-imagPart z)
  where
    showImag x
        | abs (abs x - 1) < 1e-12 = if x > 0 then "i" else "-i"
        | otherwise               = niceShow x ++ "i"
