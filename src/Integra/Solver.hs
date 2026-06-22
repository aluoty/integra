module Integra.Solver (solveLinear, solveQuadratic) where

import Integra.AST (Expr)
import Integra.Evaluator (evalWithX, showComplex)
import Data.Complex (Complex(..), magnitude)

solveLinear :: Expr -> String
solveLinear expr =
    let b = evalWithX 0 expr
        a = evalWithX 1 expr - b
    in if magnitude a < 1e-12
        then "No unique solution (coefficient of x is 0)"
        else "x = " ++ showComplex (-b / a)

solveQuadratic :: Expr -> String
solveQuadratic expr =
    let c = evalWithX 0 expr
        b = (evalWithX 1 expr - evalWithX (-1) expr) / (2 :+ 0)
        a = evalWithX 1 expr - b - c
        d = b*b - 4*a*c
    in if magnitude a < 1e-12
        then solveLinear expr
        else let sqrtD = sqrt d
                 x1 = (-b + sqrtD) / (2*a)
                 x2 = (-b - sqrtD) / (2*a)
             in "x = " ++ showComplex x1 ++ "\nx = " ++ showComplex x2
