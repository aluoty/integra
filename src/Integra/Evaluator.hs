module Integra.Evaluator (eval, evalWithX, niceShow, showComplex) where

import Data.Complex
import Integra.AST (Expr(..))
import Integra.Special (gamma, erf)

eval :: Complex Double -> Complex Double -> Expr -> Complex Double
eval _ _   (Num n)      = n :+ 0
eval _ _   Pi           = pi :+ 0
eval _ _   E            = exp 1 :+ 0
eval _ _   Tau          = (2 * pi) :+ 0
eval _ _   Phi          = ((1 + sqrt 5) / 2) :+ 0
eval _ _   I            = 0 :+ 1
eval x _   Var          = x
eval _ ans Ans          = ans

eval x ans (Add a b)    = eval x ans a + eval x ans b
eval x ans (Sub a b)    = eval x ans a - eval x ans b
eval x ans (Mul a b)    = eval x ans a * eval x ans b
eval x ans (Div a b)    = eval x ans a / eval x ans b
eval x ans (Pow a b)    = eval x ans a ** eval x ans b

eval x ans (Gt  a b)    = let va = eval x ans a; vb = eval x ans b in if realPart va >  realPart vb then 1 else 0
eval x ans (Ge  a b)    = let va = eval x ans a; vb = eval x ans b in if realPart va >= realPart vb then 1 else 0
eval x ans (Lt  a b)    = let va = eval x ans a; vb = eval x ans b in if realPart va <  realPart vb then 1 else 0
eval x ans (Le  a b)    = let va = eval x ans a; vb = eval x ans b in if realPart va <= realPart vb then 1 else 0
eval x ans (Eqq a b)    = let va = eval x ans a; vb = eval x ans b in if realPart va == realPart vb then 1 else 0
eval x ans (Neq a b)    = let va = eval x ans a; vb = eval x ans b in if realPart va /= realPart vb then 1 else 0

eval x ans (SinE   a)   = sin      (eval x ans a)
eval x ans (CosE   a)   = cos      (eval x ans a)
eval x ans (TanE   a)   = tan      (eval x ans a)
eval x ans (CscE   a)   = recip    (sin (eval x ans a))
eval x ans (SecE   a)   = recip    (cos (eval x ans a))
eval x ans (CotE   a)   = recip    (tan (eval x ans a))
eval x ans (AsinE  a)   = asin     (eval x ans a)
eval x ans (AcosE  a)   = acos     (eval x ans a)
eval x ans (AtanE  a)   = atan     (eval x ans a)
eval x ans (AcscE  a)   = asin     (recip (eval x ans a))
eval x ans (AsecE  a)   = acos     (recip (eval x ans a))
eval x ans (AcotE  a)   = atan     (recip (eval x ans a))
eval x ans (SinhE  a)   = sinh     (eval x ans a)
eval x ans (CoshE  a)   = cosh     (eval x ans a)
eval x ans (TanhE  a)   = tanh     (eval x ans a)
eval x ans (CschE  a)   = recip    (sinh (eval x ans a))
eval x ans (SechE  a)   = recip    (cosh (eval x ans a))
eval x ans (CothE  a)   = recip    (tanh (eval x ans a))
eval x ans (AsinhE a)   = asinh    (eval x ans a)
eval x ans (AcoshE a)   = acosh    (eval x ans a)
eval x ans (AtanhE a)   = atanh    (eval x ans a)
eval x ans (LogE   a)   = log      (eval x ans a)
eval x ans (Log2E  a)   = logBase2 (eval x ans a)
eval x ans (Log10E a)   = logBase10(eval x ans a)
eval x ans (Log1pE a)   = log (1 + eval x ans a)
eval x ans (ExpE   a)   = exp      (eval x ans a)
eval x ans (Expm1E a)   = exp (eval x ans a) - 1
eval x ans (SqrtE  a)   = sqrt     (eval x ans a)
eval x ans (CbrtE  a)   = (eval x ans a) ** (1/3 :+ 0)
eval x ans (AbsE   a)   = magnitude (eval x ans a) :+ 0
eval x ans (SignE  a)   = let v = eval x ans a in signum v
eval x ans (FloorE a)   = fromIntegral (floor (realPart (eval x ans a))) :+ 0
eval x ans (CeilE  a)   = fromIntegral (ceiling (realPart (eval x ans a))) :+ 0
eval x ans (RoundE a)   = fromIntegral (round (realPart (eval x ans a))) :+ 0
eval x ans (GammaE a)   = gamma (eval x ans a)
eval x ans (ErfE   a)   = erf (eval x ans a)
eval x ans (ConjE  a)   = conjugate (eval x ans a)
eval x ans (ReE    a)   = realPart (eval x ans a) :+ 0
eval x ans (ImE    a)   = imagPart (eval x ans a) :+ 0

evalWithX :: Double -> Expr -> Complex Double
evalWithX x e = eval (x :+ 0) (0 :+ 0) e

logBase2 :: Complex Double -> Complex Double
logBase2 z = log z / log (2 :+ 0)

logBase10 :: Complex Double -> Complex Double
logBase10 z = log z / log (10 :+ 0)

showComplex :: Complex Double -> String
showComplex z
    | imagPart z == 0 = niceShow (realPart z)
    | realPart z == 0 = let im = niceShow (imagPart z)
                        in if im == "1" then "i" else if im == "-1" then "-i" else im ++ "i"
    | otherwise       = let im = niceShow (abs (imagPart z))
                            imStr = if im == "1" then "i" else im ++ "i"
                        in niceShow (realPart z) ++ (if imagPart z > 0 then "+" else "-") ++ imStr

niceShow :: Double -> String
niceShow n
    | isNaN n      = "undefined"
    | isInfinite n = if n > 0 then "∞" else "-∞"
    | n == 0       = "0"
    | abs n >= 1e12 || (abs n < 1e-8 && abs n > 0) = show n
    | otherwise    = let s = show n
                         s' = if '.' `elem` s
                              then reverse (dropWhile (== '0') (reverse s))
                              else s
                     in if last s' == '.' then init s' else s'
