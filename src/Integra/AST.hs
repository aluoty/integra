module Integra.AST (Expr(..)) where

data Expr
    = Num    Double
    | Add    Expr Expr
    | Sub    Expr Expr
    | Mul    Expr Expr
    | Div    Expr Expr
    | Pow    Expr Expr
    | Var
    | Ans
    | SinE   Expr
    | CosE   Expr
    | TanE   Expr
    | AsinE  Expr
    | AcosE  Expr
    | AtanE  Expr
    | SinhE  Expr
    | CoshE  Expr
    | TanhE  Expr
    | LogE   Expr
    | ExpE   Expr
    | SqrtE  Expr
    | AbsE   Expr
    | FloorE Expr
    | CeilE  Expr
    | RoundE Expr
    | Pi
    | E
    deriving (Show)
