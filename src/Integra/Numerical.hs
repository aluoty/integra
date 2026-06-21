module Integra.Numerical (derivative, integral) where

derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + h) - f (x - h)) / (2 * h)
  where h = 1e-8

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
