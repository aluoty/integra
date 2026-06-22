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
    | SinE   Expr | CosE   Expr | TanE   Expr
    | CscE   Expr | SecE   Expr | CotE   Expr
    | AsinE  Expr | AcosE  Expr | AtanE  Expr
    | AcscE  Expr | AsecE  Expr | AcotE  Expr
    | SinhE  Expr | CoshE  Expr | TanhE  Expr
    | CschE  Expr | SechE  Expr | CothE  Expr
    | AsinhE Expr | AcoshE Expr | AtanhE Expr
    | LogE   Expr | Log2E  Expr | Log10E Expr
    | ExpE   Expr | SqrtE  Expr
    | AbsE   Expr | SignE  Expr
    | FloorE Expr | CeilE  Expr | RoundE Expr
    | GammaE Expr | ErfE   Expr
    | ConjE  Expr | ReE    Expr | ImE    Expr
    | Pi | E | Tau | Phi | I
    deriving (Show)
