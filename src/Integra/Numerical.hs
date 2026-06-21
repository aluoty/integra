module Integra.Numerical
    ( derivative, derivative2, derivativeN
    , integral
    , limit
    , taylorSeries
    ) where

derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + h) - f (x - h)) / (2 * h)
  where h = 1e-8

derivative2 :: (Double -> Double) -> Double -> Double
derivative2 f x = (f (x - h) - 2 * f x + f (x + h)) / (h * h)
  where h = 1e-5

derivativeN :: Int -> (Double -> Double) -> Double -> Double
derivativeN 0 f x = f x
derivativeN 1 f x = derivative f x
derivativeN 2 f x = derivative2 f x
derivativeN n f x =
    let h = max 1e-8 (1e-3 / fromIntegral n)
    in ((derivativeN (n-1) f) (x + h) - (derivativeN (n-1) f) (x - h)) / (2 * h)

integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = simpson (1000 :: Int)
  where
    simpson n =
        let h  = (b - a) / fromIntegral n
            x i = a + fromIntegral i * h
            s0 = f a + f b
            s1 = sum [f (x i) | i <- [1,3..n-1]]
            s2 = sum [f (x i) | i <- [2,4..n-2]]
        in h / 3 * (s0 + 4 * s1 + 2 * s2)

limit :: (Double -> Double) -> Double -> Double
limit f a = f (a + h)
  where h = 1e-10

taylorSeries :: (Double -> Double) -> Double -> Int -> Double -> Double
taylorSeries f a n x =
    sum [taylorTerm i * (x - a) ** fromIntegral i | i <- [0..n]]
  where
    taylorTerm 0 = f a
    taylorTerm k = derivativeN k f a / product [1 .. fromIntegral k]
