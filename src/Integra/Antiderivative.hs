module Integra.Antiderivative (findAntiderivative) where

import Integra.AST (Expr(..))

findAntiderivative :: Expr -> Expr
findAntiderivative (Num n)       = Mul (Num n) Var
findAntiderivative (Add a b)     = Add (findAntiderivative a) (findAntiderivative b)
findAntiderivative (Sub a b)     = Sub (findAntiderivative a) (findAntiderivative b)
findAntiderivative (Mul (Num n) (CosE (Mul (Num k) Var)))
    | k /= 0 = Div (Mul (Num n) (SinE (Mul (Num k) Var))) (Num k)
findAntiderivative (Mul (Num n) (SinE (Mul (Num k) Var)))
    | k /= 0 = Div (Mul (Num n) (Sub (Num 0) (CosE (Mul (Num k) Var)))) (Num k)
findAntiderivative (Mul (Num n) (ExpE (Mul (Num k) Var)))
    | k /= 0 = Div (Mul (Num n) (ExpE (Mul (Num k) Var))) (Num k)
findAntiderivative (Pow Var (Num n))
    | n /= -1 = Div (Pow Var (Num (n+1))) (Num (n+1))
findAntiderivative (Div (Num n) Var) = Mul (Num n) (LogE (AbsE Var))
findAntiderivative _                = Mul (Num 0) Var
