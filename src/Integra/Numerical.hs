module Integra.Numerical
    ( derivative, derivative2, derivativeN
    , integral
    , limit
    , taylorSeries
    ) where

import Data.Complex (Complex(..))

derivative :: (Double -> Complex Double) -> Double -> Complex Double
derivative f x = (f (x + h) - f (x - h)) / ((2 * h) :+ 0)
  where h = 1e-8

derivative2 :: (Double -> Complex Double) -> Double -> Complex Double
derivative2 f x = (f (x - h) - 2 * f x + f (x + h)) / ((h * h) :+ 0)
  where h = 1e-5

derivativeN :: Int -> (Double -> Complex Double) -> Double -> Complex Double
derivativeN 0 f x = f x
derivativeN 1 f x = derivative f x
derivativeN 2 f x = derivative2 f x
derivativeN n f x =
    let h = max 1e-8 (1e-3 / fromIntegral n)
    in ((derivativeN (n-1) f) (x + h) - (derivativeN (n-1) f) (x - h)) / ((2 * h) :+ 0)

integral :: (Double -> Complex Double) -> Double -> Double -> Complex Double
integral f a b = simpson (if n < 2 then 2 else n)
  where
    n = max 2 (1000 :: Int)
    simpson n' =
        let h  = (b - a) / fromIntegral n'
            xi i = a + fromIntegral i * h
            s0 = f a + f b
            s1 = sum [f (xi i) | i <- [1,3..n'-1]]
            s2 = sum [f (xi i) | i <- [2,4..n'-2]]
        in ((h / 3) :+ 0) * (s0 + 4 * s1 + 2 * s2)

limit :: (Double -> Complex Double) -> Double -> Complex Double
limit f a = f (a + h)
  where h = 1e-10

taylorSeries :: (Double -> Complex Double) -> Double -> Int -> Double -> Complex Double
taylorSeries f a n x =
    sum [taylorTerm i * ((x - a) :+ 0) ** (fromIntegral i :+ 0) | i <- [0..n]]
  where
    taylorTerm 0 = f a
    taylorTerm k = derivativeN k f a / (product [1 .. fromIntegral k] :+ 0)
