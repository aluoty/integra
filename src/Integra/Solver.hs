module Integra.Solver (solveLinear, solveQuadratic) where

import Integra.AST (Expr)
import Integra.Evaluator (evalWithX)

solveLinear :: Expr -> String
solveLinear expr =
    let b = evalWithX 0 expr
        a = evalWithX 1 expr - b
    in if abs a < 1e-12
        then "No unique solution (coefficient of x is 0)"
        else "x = " ++ show (-b / a)

solveQuadratic :: Expr -> String
solveQuadratic expr =
    let c = evalWithX 0 expr
        b = (evalWithX 1 expr - evalWithX (-1) expr) / 2
        a = evalWithX 1 expr - b - c
        d = b * b - 4 * a * c
    in if abs a < 1e-12
        then solveLinear expr
        else if d < 0
            then let re = -b / (2 * a)
                     im = sqrt (-d) / (2 * a)
                 in "x = " ++ show re ++ " + " ++ show im ++ "i"
                 ++ "\nx = " ++ show re ++ " - " ++ show im ++ "i"
            else let x1 = (-b + sqrt d) / (2 * a)
                     x2 = (-b - sqrt d) / (2 * a)
                 in "x = " ++ show x1 ++ "\nx = " ++ show x2
