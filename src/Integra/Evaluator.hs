module Integra.Evaluator (eval, evalWithX) where

import Integra.AST (Expr(..))
import Integra.Special (gamma, erf)

eval :: Double -> Double -> Expr -> Double
eval x ansVal = go
  where
    go (Num n)       = n
    go Pi            = pi
    go E             = exp 1
    go Tau           = 2 * pi
    go Phi           = (1 + sqrt 5) / 2
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
    go (LogE   e)    = log (go e)
    go (Log2E  e)    = log (go e) / log 2
    go (Log10E e)    = log (go e) / log 10
    go (ExpE   e)    = exp (go e)
    go (SqrtE  e)    = sqrt (go e)
    go (AbsE   e)    = abs (go e)
    go (SignE  e)    = signum (go e)
    go (FloorE e)    = fromIntegral (floor (go e) :: Int)
    go (CeilE  e)    = fromIntegral (ceiling (go e) :: Int)
    go (RoundE e)    = fromIntegral (round (go e) :: Int)
    go (GammaE e)    = gamma (go e)
    go (ErfE   e)    = erf (go e)

evalWithX :: Double -> Expr -> Double
evalWithX x = eval x 0
