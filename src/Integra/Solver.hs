module Integra.Solver (solveLinear, solveQuadratic, solveCubic, maybeShowRoots) where

import Data.Complex
import Data.List (intercalate)
import Integra.AST (Expr(..))
import Integra.Evaluator (eval, showComplex)
import Integra.Derive (hasVar)

evalWithX :: Double -> Expr -> Complex Double
evalWithX x e = eval (x :+ 0) (0 :+ 0) e

eps :: Double
eps = 1e-12

solveLinear :: Expr -> String
solveLinear (Eqq l r) = solveLinear (Sub l r)
solveLinear expr
    | not (hasVar expr) = "No variable x found in expression"
    | otherwise =
        let b = evalWithX 0 expr
            a = evalWithX 1 expr - b
        in if magnitude a < eps
           then if magnitude b < eps
                then "Infinite solutions (identity)"
                else "No solution"
           else "x = " ++ showComplex ((-b) / a)

solveQuadratic :: Expr -> String
solveQuadratic (Eqq l r) = solveQuadratic (Sub l r)
solveQuadratic expr
    | not (hasVar expr) = "No variable x found in expression"
    | otherwise =
        let c = evalWithX 0 expr
            b = (evalWithX 1 expr - evalWithX (-1) expr) / 2
            a = evalWithX 1 expr - b - c
        in if magnitude a < eps
           then solveLinear expr
           else let disc = b*b - 4*a*c
                    sqrtDisc = sqrt disc
                    x1 = (-b + sqrtDisc) / (2*a)
                    x2 = (-b - sqrtDisc) / (2*a)
                in if magnitude disc < eps
                   then "x = " ++ showComplex x1 ++ " (repeated)"
                   else "x₁ = " ++ showComplex x1 ++ "\nx₂ = " ++ showComplex x2

solveCubic :: Expr -> String
solveCubic (Eqq l r) = solveCubic (Sub l r)
solveCubic expr
    | not (hasVar expr) = "No variable x found in expression"
    | otherwise =
        let d = evalWithX 0 expr
            f1 = evalWithX 1 expr
            f_1 = evalWithX (-1) expr
            f2 = evalWithX 2 expr
            b = (f1 + f_1) / 2 - d
            ac = (f1 - f_1) / 2
            a = (f2 - 4*b - 2*ac - d) / 6
            c' = ac - a
        in if magnitude a < eps
           then solveQuadratic expr
           else showCubicRoots a b c' d

showCubicRoots :: Complex Double -> Complex Double -> Complex Double -> Complex Double -> String
showCubicRoots a b c d =
    let p  = (3*a*c - b*b) / (3*a*a)
        q  = (2*b*b*b - 9*a*b*c + 27*a*a*d) / (27*a*a*a)
        disc = (q/2)**2 + (p/3)**3
        sqrtDisc = sqrt disc
        u = (-q/2 + sqrtDisc) ** (1/3 :+ 0)
        v = (-q/2 - sqrtDisc) ** (1/3 :+ 0)
        omega = (-0.5) :+ sqrt 3.0 / 2
        omega2 = omega * omega
        offset = b / (3*a)
        x1 = u + v - offset
        x2 = u*omega + v*omega2 - offset
        x3 = u*omega2 + v*omega - offset
    in if magnitude (x1 - x2) < eps && magnitude (x2 - x3) < eps
       then "x = " ++ showComplex x1 ++ " (triple)"
       else if magnitude (x1 - x2) < eps || magnitude (x2 - x3) < eps
            then let repeated = x1
                     single = if magnitude (x1 - x2) < eps then x3 else x1
                 in "x₁ = " ++ showComplex repeated ++ " (repeated)\nx₂ = " ++ showComplex single
            else "x₁ = " ++ showComplex x1 ++ "\nx₂ = " ++ showComplex x2 ++ "\nx₃ = " ++ showComplex x3

maybeShowRoots :: Expr -> Maybe String
maybeShowRoots (Pow (Num a) (Pow (Num n) (Num (-1)))) = maybeShowRoots (Pow (Num a) (Div (Num 1) (Num n)))
maybeShowRoots (Pow (Num a) (Div (Num 1) (Num n)))
    | n > 1 && abs (fromIntegral (round n) - n) < eps =
        let ni = round n :: Int
            r  = abs a ** (1/n)
            theta = if a >= 0 then 0 else pi
            roots = [ showComplex ((r :+ 0) * cis ((theta + 2*pi*fromIntegral k)/n)) | k <- [0..ni-1] ]
        in Just ("All " ++ show ni ++ " roots:\n  " ++ intercalate "\n  " roots)
maybeShowRoots _ = Nothing
