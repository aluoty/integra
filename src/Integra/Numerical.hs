module Integra.Numerical
    ( adaptSimpson, findRoot
    , derivative, derivative2, derivativeN
    , limit, taylorSeries
    ) where

import Data.Complex

-- | Adaptive Simpson's quadrature for functions f: Double -> Complex Double
adaptSimpson :: (Double -> Complex Double) -> Double -> Double -> Complex Double
adaptSimpson f a b = adaptSimpson' f a b (simpson f a b) 1e-8
  where
    adaptSimpson' f a b whole tol
        | magnitude (left + right - whole) < 15 * tol = left + right + (left + right - whole) / 15
        | otherwise = adaptSimpson' f a m left (tol/2) + adaptSimpson' f m b right (tol/2)
      where
        m = (a + b) / 2
        left  = simpson f a m
        right = simpson f m b

simpson :: (Double -> Complex Double) -> Double -> Double -> Complex Double
simpson f a b = (h/3 :+ 0) * (f a + 4 * f m + f b)
  where
    m = (a + b) / 2
    h = (b - a) / 2

-- | Find root of f(x) = 0 using Newton's method, x as complex
findRoot :: (Complex Double -> Complex Double) -> Complex Double -> Complex Double
findRoot f x0 = go x0 0
  where
    go x n
        | magnitude (f x) < 1e-12 = x
        | n > 1000 = x
        | otherwise = go (x - f x / deriv x) (n + 1)
      where
        deriv x' = (f (x' + dx) - f (x' - dx)) / (2 * dx)
        dx = 1e-8 :+ 1e-8

-- | Numerical first derivative (central difference)
derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + h) - f (x - h)) / (2 * h)
  where h = 1e-8

-- | Numerical second derivative
derivative2 :: (Double -> Double) -> Double -> Double
derivative2 f x = (f (x - h) - 2 * f x + f (x + h)) / (h * h)
  where h = 1e-5

-- | Numerical nth derivative (recursive)
derivativeN :: Int -> (Double -> Double) -> Double -> Double
derivativeN 0 f x = f x
derivativeN 1 f x = derivative f x
derivativeN 2 f x = derivative2 f x
derivativeN n f x =
    let h = max 1e-8 (1e-3 / fromIntegral n)
    in ((derivativeN (n-1) f) (x + h) - (derivativeN (n-1) f) (x - h)) / (2 * h)

-- | Numerical limit (evaluate at x + epsilon)
limit :: (Double -> Double) -> Double -> Double
limit f a = f (a + h)
  where h = 1e-10

-- | Taylor series approximation at x = 0, expanded around a
taylorSeries :: (Double -> Double) -> Double -> Int -> Double -> Double
taylorSeries f a n x =
    sum [taylorTerm i * (x - a) ** fromIntegral i | i <- [0..n]]
  where
    taylorTerm 0 = f a
    taylorTerm k = derivativeN k f a / product [1 .. fromIntegral k]
