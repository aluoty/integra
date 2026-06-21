module Integra.Evaluator (eval, evalWithX) where

import Integra.AST (Expr(..))

eval :: Double -> Double -> Expr -> Double
eval x ansVal = go
  where
    go (Num n)      = n
    go Pi           = pi
    go E            = exp 1
    go Var          = x
    go Ans          = ansVal
    go (Add l r)    = go l + go r
    go (Sub l r)    = go l - go r
    go (Mul l r)    = go l * go r
    go (Div l r)    = go l / go r
    go (Pow l r)    = go l ** go r
    go (SinE e)     = sin (go e)
    go (CosE e)     = cos (go e)
    go (TanE e)     = tan (go e)
    go (AsinE e)    = asin (go e)
    go (AcosE e)    = acos (go e)
    go (AtanE e)    = atan (go e)
    go (SinhE e)    = sinh (go e)
    go (CoshE e)    = cosh (go e)
    go (TanhE e)    = tanh (go e)
    go (LogE e)     = log (go e)
    go (ExpE e)     = exp (go e)
    go (SqrtE e)    = sqrt (go e)
    go (AbsE e)     = abs (go e)
    go (FloorE e)   = fromIntegral (floor (go e) :: Int)
    go (CeilE e)    = fromIntegral (ceiling (go e) :: Int)
    go (RoundE e)   = fromIntegral (round (go e) :: Int)

evalWithX :: Double -> Expr -> Double
evalWithX x = eval x 0
