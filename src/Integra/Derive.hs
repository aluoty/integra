module Integra.Derive (deriv, hasVar, steps, showExpr, simplify) where

import Integra.AST (Expr(..))

-- | Symbolic differentiation with simplification
deriv :: Expr -> Expr
deriv = simplify . derivRaw

-- | Raw symbolic differentiation (unsimplified)
derivRaw :: Expr -> Expr
derivRaw (Num _)       = Num 0
derivRaw Pi            = Num 0
derivRaw E             = Num 0
derivRaw Tau           = Num 0
derivRaw Phi           = Num 0
derivRaw I             = Num 0
derivRaw Var           = Num 1
derivRaw Ans           = Num 0

derivRaw (Add a b)     = Add (derivRaw a) (derivRaw b)
derivRaw (Sub a b)     = Sub (derivRaw a) (derivRaw b)
derivRaw (Mul a b)     = Add (Mul (derivRaw a) b) (Mul a (derivRaw b))
derivRaw (Div a b)     = Div (Sub (Mul (derivRaw a) b) (Mul a (derivRaw b))) (Pow b (Num 2))
derivRaw (Pow a (Num n)) = Mul (Mul (Num n) (Pow a (Num (n-1)))) (derivRaw a)
derivRaw (Pow a b)     = Mul (Pow a b) (derivRaw (Mul b (LogE a)))
derivRaw (SinE a)      = Mul (CosE a) (derivRaw a)
derivRaw (CosE a)      = Mul (Sub (Num 0) (SinE a)) (derivRaw a)
derivRaw (TanE a)      = Mul (Add (Num 1) (Pow (TanE a) (Num 2))) (derivRaw a)
derivRaw (CscE a)      = Mul (Sub (Num 0) (Mul (CscE a) (CotE a))) (derivRaw a)
derivRaw (SecE a)      = Mul (Mul (SecE a) (TanE a)) (derivRaw a)
derivRaw (CotE a)      = Mul (Sub (Num 0) (Add (Num 1) (Pow (CotE a) (Num 2)))) (derivRaw a)
derivRaw (AsinE a)     = Div (derivRaw a) (SqrtE (Sub (Num 1) (Pow a (Num 2))))
derivRaw (AcosE a)     = Div (Sub (Num 0) (derivRaw a)) (SqrtE (Sub (Num 1) (Pow a (Num 2))))
derivRaw (AtanE a)     = Div (derivRaw a) (Add (Num 1) (Pow a (Num 2)))
derivRaw (AcscE a)     = Div (Sub (Num 0) (derivRaw a)) (Mul (AbsE a) (SqrtE (Sub (Pow a (Num 2)) (Num 1))))
derivRaw (AsecE a)     = Div (derivRaw a) (Mul (AbsE a) (SqrtE (Sub (Pow a (Num 2)) (Num 1))))
derivRaw (AcotE a)     = Div (Sub (Num 0) (derivRaw a)) (Add (Num 1) (Pow a (Num 2)))
derivRaw (SinhE a)     = Mul (CoshE a) (derivRaw a)
derivRaw (CoshE a)     = Mul (SinhE a) (derivRaw a)
derivRaw (TanhE a)     = Mul (Sub (Num 1) (Pow (TanhE a) (Num 2))) (derivRaw a)
derivRaw (CschE a)     = Mul (Sub (Num 0) (Mul (CschE a) (CothE a))) (derivRaw a)
derivRaw (SechE a)     = Mul (Sub (Num 0) (Mul (SechE a) (TanhE a))) (derivRaw a)
derivRaw (CothE a)     = Mul (Sub (Num 0) (Sub (Num 1) (Pow (CothE a) (Num 2)))) (derivRaw a)
derivRaw (AsinhE a)    = Div (derivRaw a) (SqrtE (Add (Pow a (Num 2)) (Num 1)))
derivRaw (AcoshE a)    = Div (derivRaw a) (SqrtE (Sub (Pow a (Num 2)) (Num 1)))
derivRaw (AtanhE a)    = Div (derivRaw a) (Sub (Num 1) (Pow a (Num 2)))
derivRaw (LogE a)      = Div (derivRaw a) a
derivRaw (Log2E a)     = Div (derivRaw a) (Mul a (LogE (Num 2)))
derivRaw (Log10E a)    = Div (derivRaw a) (Mul a (LogE (Num 10)))
derivRaw (Log1pE a)    = Div (derivRaw a) (Add (Num 1) a)
derivRaw (ExpE a)      = Mul (ExpE a) (derivRaw a)
derivRaw (Expm1E a)    = Mul (ExpE a) (derivRaw a)
derivRaw (SqrtE a)     = Div (derivRaw a) (Mul (Num 2) (SqrtE a))
derivRaw (CbrtE a)     = Div (derivRaw a) (Mul (Num 3) (Pow (CbrtE a) (Num 2)))
derivRaw (AbsE a)      = Mul (Div a (AbsE a)) (derivRaw a)
derivRaw (SignE a)     = Num 0
derivRaw (FloorE a)    = Num 0
derivRaw (CeilE a)     = Num 0
derivRaw (RoundE a)    = Num 0
derivRaw (GammaE a)    = Mul (GammaE a) (polygammaE a)
derivRaw (ErfE a)      = Mul (Div (Num 2) (Mul (SqrtE Pi) (ExpE (Mul (Sub (Num 0) (Num 1)) (Pow a (Num 2)))))) (derivRaw a)
derivRaw (ConjE a)     = ConjE (derivRaw a)
derivRaw (ReE a)       = ReE (derivRaw a)
derivRaw (ImE a)       = ImE (derivRaw a)
derivRaw (Gt  _ _)     = Num 0
derivRaw (Ge  _ _)     = Num 0
derivRaw (Lt  _ _)     = Num 0
derivRaw (Le  _ _)     = Num 0
derivRaw (Eqq _ _)     = Num 0
derivRaw (Neq _ _)     = Num 0

-- Placeholder for polygamma — not fully implemented
polygammaE :: Expr -> Expr
polygammaE _ = Num 0

-- | Simplify expressions by removing trivial operations
simplify :: Expr -> Expr
simplify (Mul (Num 1) b)                 = simplify b
simplify (Mul a (Num 1))                 = simplify a
simplify (Mul (Num 0) _)                 = Num 0
simplify (Mul _ (Num 0))                 = Num 0
simplify (Mul (Num a) (Num b))           = Num (a*b)
simplify (Mul a b)                       = case (simplify a, simplify b) of
    (Num 1, s) -> s
    (s, Num 1) -> s
    (Num 0, _) -> Num 0
    (_, Num 0) -> Num 0
    (Num x, Num y) -> Num (x*y)
    (Num x, Mul (Num y) c) -> simplify (Mul (Num (x*y)) c)
    (Mul (Num x) c, Num y) -> simplify (Mul (Num (x*y)) c)
    (sa, sb)   -> Mul sa sb
simplify (Add (Num 0) b)                 = simplify b
simplify (Add a (Num 0))                 = simplify a
simplify (Add (Num a) (Num b))           = Num (a+b)
simplify (Add a b)                       = case (simplify a, simplify b) of
    (Num 0, s) -> s
    (s, Num 0) -> s
    (Num x, Num y) -> Num (x+y)
    (sa, sb)   -> Add sa sb
simplify (Sub (Num a) (Num b))           = Num (a-b)
simplify (Sub a (Num 0))                 = simplify a
simplify (Sub (Num 0) b)                 = simplify (Mul (Num (-1)) b)
simplify (Sub a b)                       = case (simplify a, simplify b) of
    (s, Num 0) -> s
    (Num x, Num y) -> Num (x-y)
    (sa, sb)   -> Sub sa sb
simplify (Div (Num 0) _)                 = Num 0
simplify (Div a (Num 1))                 = simplify a
simplify (Div (Num a) (Num b))           = Num (a/b)
simplify (Div a b)                       = case (simplify a, simplify b) of
    (Num 0, _) -> Num 0
    (s, Num 1) -> s
    (Num x, Num y) -> Num (x/y)
    (sa, sb)   -> Div sa sb
simplify (Pow (Num a) (Num b))           = Num (a**b)
simplify (Pow _ (Num 0))                 = Num 1
simplify (Pow a (Num 1))                 = simplify a
simplify (Pow a b)                       = case (simplify a, simplify b) of
    (_, Num 0) -> Num 1
    (s, Num 1) -> s
    (Num x, Num y) -> Num (x**y)
    (sa, sb)   -> Pow sa sb
simplify (SinE a)                        = SinE (simplify a)
simplify (CosE a)                        = CosE (simplify a)
simplify (TanE a)                        = TanE (simplify a)
simplify (CscE a)                        = CscE (simplify a)
simplify (SecE a)                        = SecE (simplify a)
simplify (CotE a)                        = CotE (simplify a)
simplify (AsinE a)                       = AsinE (simplify a)
simplify (AcosE a)                       = AcosE (simplify a)
simplify (AtanE a)                       = AtanE (simplify a)
simplify (AcscE a)                       = AcscE (simplify a)
simplify (AsecE a)                       = AsecE (simplify a)
simplify (AcotE a)                       = AcotE (simplify a)
simplify (SinhE a)                       = SinhE (simplify a)
simplify (CoshE a)                       = CoshE (simplify a)
simplify (TanhE a)                       = TanhE (simplify a)
simplify (CschE a)                       = CschE (simplify a)
simplify (SechE a)                       = SechE (simplify a)
simplify (CothE a)                       = CothE (simplify a)
simplify (AsinhE a)                      = AsinhE (simplify a)
simplify (AcoshE a)                      = AcoshE (simplify a)
simplify (AtanhE a)                      = AtanhE (simplify a)
simplify (LogE E)                        = Num 1
simplify (LogE (Num 1))                  = Num 0
simplify (LogE a)                        = LogE (simplify a)
simplify (Log2E (Num 2))                 = Num 1
simplify (Log2E a)                       = Log2E (simplify a)
simplify (Log10E (Num 10))               = Num 1
simplify (Log10E a)                      = Log10E (simplify a)
simplify (Log1pE a)                      = Log1pE (simplify a)
simplify (ExpE a)                        = ExpE (simplify a)
simplify (Expm1E a)                      = Expm1E (simplify a)
simplify (SqrtE a)                       = SqrtE (simplify a)
simplify (CbrtE a)                       = CbrtE (simplify a)
simplify (AbsE a)                        = AbsE (simplify a)
simplify (SignE a)                       = SignE (simplify a)
simplify (FloorE a)                      = FloorE (simplify a)
simplify (CeilE a)                       = CeilE (simplify a)
simplify (RoundE a)                      = RoundE (simplify a)
simplify (GammaE a)                      = GammaE (simplify a)
simplify (ErfE a)                        = ErfE (simplify a)
simplify (ConjE a)                       = ConjE (simplify a)
simplify (ReE a)                         = ReE (simplify a)
simplify (ImE a)                         = ImE (simplify a)
simplify (Gt a b)                        = Gt (simplify a) (simplify b)
simplify (Ge a b)                        = Ge (simplify a) (simplify b)
simplify (Lt a b)                        = Lt (simplify a) (simplify b)
simplify (Le a b)                        = Le (simplify a) (simplify b)
simplify (Eqq a b)                       = Eqq (simplify a) (simplify b)
simplify (Neq a b)                       = Neq (simplify a) (simplify b)
simplify e                               = e

-- | Check if expression contains variable
hasVar :: Expr -> Bool
hasVar Var            = True
hasVar Ans            = False
hasVar (Num _)        = False
hasVar Pi             = False
hasVar E              = False
hasVar Tau            = False
hasVar Phi            = False
hasVar I              = False
hasVar (Add a b)      = hasVar a || hasVar b
hasVar (Sub a b)      = hasVar a || hasVar b
hasVar (Mul a b)      = hasVar a || hasVar b
hasVar (Div a b)      = hasVar a || hasVar b
hasVar (Pow a b)      = hasVar a || hasVar b
hasVar (Gt a b)       = hasVar a || hasVar b
hasVar (Ge a b)       = hasVar a || hasVar b
hasVar (Lt a b)       = hasVar a || hasVar b
hasVar (Le a b)       = hasVar a || hasVar b
hasVar (Eqq a b)      = hasVar a || hasVar b
hasVar (Neq a b)      = hasVar a || hasVar b
hasVar (SinE a)       = hasVar a
hasVar (CosE a)       = hasVar a
hasVar (TanE a)       = hasVar a
hasVar (CscE a)       = hasVar a
hasVar (SecE a)       = hasVar a
hasVar (CotE a)       = hasVar a
hasVar (AsinE a)      = hasVar a
hasVar (AcosE a)      = hasVar a
hasVar (AtanE a)      = hasVar a
hasVar (AcscE a)      = hasVar a
hasVar (AsecE a)      = hasVar a
hasVar (AcotE a)      = hasVar a
hasVar (SinhE a)      = hasVar a
hasVar (CoshE a)      = hasVar a
hasVar (TanhE a)      = hasVar a
hasVar (CschE a)      = hasVar a
hasVar (SechE a)      = hasVar a
hasVar (CothE a)      = hasVar a
hasVar (AsinhE a)     = hasVar a
hasVar (AcoshE a)     = hasVar a
hasVar (AtanhE a)     = hasVar a
hasVar (LogE a)       = hasVar a
hasVar (Log2E a)      = hasVar a
hasVar (Log10E a)     = hasVar a
hasVar (Log1pE a)     = hasVar a
hasVar (ExpE a)       = hasVar a
hasVar (Expm1E a)     = hasVar a
hasVar (SqrtE a)      = hasVar a
hasVar (CbrtE a)      = hasVar a
hasVar (AbsE a)       = hasVar a
hasVar (SignE a)      = hasVar a
hasVar (FloorE a)     = hasVar a
hasVar (CeilE a)      = hasVar a
hasVar (RoundE a)     = hasVar a
hasVar (GammaE a)     = hasVar a
hasVar (ErfE a)       = hasVar a
hasVar (ConjE a)      = hasVar a
hasVar (ReE a)        = hasVar a
hasVar (ImE a)        = hasVar a

-- | Generate step-by-step explanation for derivRawative
steps :: Expr -> [(String, Expr)]
steps expr = [("Original expression", expr), ("Apply derivRawative rules", derivRaw expr)]

-- | Pretty-print an expression as a string
showExpr :: Expr -> String
showExpr (Num n)        = if n == fromIntegral (round n) then show (round n :: Integer) else show n
showExpr Pi             = "π"
showExpr E              = "e"
showExpr Tau            = "τ"
showExpr Phi            = "φ"
showExpr I              = "i"
showExpr Var            = "x"
showExpr Ans            = "ans"
showExpr (Add a b)      = showExpr a ++ " + " ++ showExpr b
showExpr (Sub a b)      = showExpr a ++ " - " ++ showExprParen b
showExpr (Mul (Num (-1)) b) = "-" ++ showExprFactor b
showExpr (Mul a (Num (-1))) = "-" ++ showExprFactor a
showExpr (Mul (Num n) b)
    | n == fromIntegral (round n) = show (round n :: Integer) ++ showExprFactor b
    | otherwise = show n ++ showExprFactor b
showExpr (Mul a (Num n))
    | n == fromIntegral (round n) = show (round n :: Integer) ++ showExprFactor a
    | otherwise = show n ++ showExprFactor a
showExpr (Mul a b)      = showExprFactor a ++ showExprFactor b
showExpr (Div a b)      = showExpr a ++ "/" ++ showExprParen b
showExpr (Pow a b)      = showExprFactor a ++ "^" ++ showExprFactor b
showExpr (Gt a b)       = showExpr a ++ " > " ++ showExpr b
showExpr (Ge a b)       = showExpr a ++ " >= " ++ showExpr b
showExpr (Lt a b)       = showExpr a ++ " < " ++ showExpr b
showExpr (Le a b)       = showExpr a ++ " <= " ++ showExpr b
showExpr (Eqq a b)      = showExpr a ++ " == " ++ showExpr b
showExpr (Neq a b)      = showExpr a ++ " != " ++ showExpr b
showExpr (SinE a)       = "sin("  ++ showExpr a ++ ")"
showExpr (CosE a)       = "cos("  ++ showExpr a ++ ")"
showExpr (TanE a)       = "tan("  ++ showExpr a ++ ")"
showExpr (CscE a)       = "csc("  ++ showExpr a ++ ")"
showExpr (SecE a)       = "sec("  ++ showExpr a ++ ")"
showExpr (CotE a)       = "cot("  ++ showExpr a ++ ")"
showExpr (AsinE a)      = "asin(" ++ showExpr a ++ ")"
showExpr (AcosE a)      = "acos(" ++ showExpr a ++ ")"
showExpr (AtanE a)      = "atan(" ++ showExpr a ++ ")"
showExpr (AcscE a)      = "acsc(" ++ showExpr a ++ ")"
showExpr (AsecE a)      = "asec(" ++ showExpr a ++ ")"
showExpr (AcotE a)      = "acot(" ++ showExpr a ++ ")"
showExpr (SinhE a)      = "sinh("  ++ showExpr a ++ ")"
showExpr (CoshE a)      = "cosh("  ++ showExpr a ++ ")"
showExpr (TanhE a)      = "tanh("  ++ showExpr a ++ ")"
showExpr (CschE a)      = "csch("  ++ showExpr a ++ ")"
showExpr (SechE a)      = "sech("  ++ showExpr a ++ ")"
showExpr (CothE a)      = "coth("  ++ showExpr a ++ ")"
showExpr (AsinhE a)     = "asinh(" ++ showExpr a ++ ")"
showExpr (AcoshE a)     = "acosh(" ++ showExpr a ++ ")"
showExpr (AtanhE a)     = "atanh(" ++ showExpr a ++ ")"
showExpr (LogE a)       = "ln("   ++ showExpr a ++ ")"
showExpr (Log2E a)      = "log2(" ++ showExpr a ++ ")"
showExpr (Log10E a)     = "log10("++ showExpr a ++ ")"
showExpr (Log1pE a)     = "log1p("++ showExpr a ++ ")"
showExpr (ExpE a)       = "exp("  ++ showExpr a ++ ")"
showExpr (Expm1E a)     = "expm1("++ showExpr a ++ ")"
showExpr (SqrtE a)      = "sqrt(" ++ showExpr a ++ ")"
showExpr (CbrtE a)      = "cbrt(" ++ showExpr a ++ ")"
showExpr (AbsE a)       = "abs("  ++ showExpr a ++ ")"
showExpr (SignE a)      = "sign(" ++ showExpr a ++ ")"
showExpr (FloorE a)     = "floor("++ showExpr a ++ ")"
showExpr (CeilE a)      = "ceil(" ++ showExpr a ++ ")"
showExpr (RoundE a)     = "round("++ showExpr a ++ ")"
showExpr (GammaE a)     = "Γ("   ++ showExpr a ++ ")"
showExpr (ErfE a)       = "erf("  ++ showExpr a ++ ")"
showExpr (ConjE a)      = "conj(" ++ showExpr a ++ ")"
showExpr (ReE a)        = "re("   ++ showExpr a ++ ")"
showExpr (ImE a)        = "im("   ++ showExpr a ++ ")"

showExprParen :: Expr -> String
showExprParen e@(Add _ _) = "(" ++ showExpr e ++ ")"
showExprParen e@(Sub _ _) = "(" ++ showExpr e ++ ")"
showExprParen e           = showExpr e

showExprFactor :: Expr -> String
showExprFactor e@(Add _ _) = "(" ++ showExpr e ++ ")"
showExprFactor e@(Sub _ _) = "(" ++ showExpr e ++ ")"
showExprFactor e           = showExpr e
