module Integra.Derive (derive, showDeriveSteps, showExpr) where

import Integra.AST (Expr(..))

derive :: Expr -> (Expr, [String])
derive expr =
    let (result, steps) = go expr
    in (result, ["f(x) = " ++ showExpr expr] ++ steps ++ ["f'(x) = " ++ showExpr result])

go :: Expr -> (Expr, [String])
go (Num _)       = (Num 0, ["  Constant rule: d/dx[c] = 0"])
go Var           = (Num 1, ["  Variable rule: d/dx[x] = 1"])
go (Add l r)     = binDeriv "+" l r Add
go (Sub l r)     = binDeriv "-" l r Sub
go (Mul l r)     =
    let (dl, sl) = go l
        (dr, sr) = go r
    in (Add (Mul dl r) (Mul l dr),
        sl ++ sr ++ ["  Product rule: (f·g)' = f'·g + f·g'"])
go (Div l r)     =
    let (dl, sl) = go l
        (dr, sr) = go r
    in (Div (Sub (Mul dl r) (Mul l dr)) (Pow r (Num 2)),
        sl ++ sr ++ ["  Quotient rule: (f/g)' = (f'·g - f·g')/g²"])
go (Pow (Num n) Var) =
    (Mul (Num n) (Pow Var (Num (n-1))),
     ["  Power rule: d/dx[x^n] = n·x^(n-1), with n = " ++ showNum n])
go (Pow Var (Num n)) =
    (Mul (Num n) (Pow Var (Num (n-1))),
     ["  Power rule: d/dx[x^n] = n·x^(n-1), with n = " ++ showNum n])
go (Pow l r)     =
    let (dl, sl) = go l
        (dr, sr) = go r
    in (Mul (Pow l r) (Add (Mul dr (LogE l)) (Mul (Div dl l) r)),
        sl ++ sr ++ ["  General power rule: (f^g)' = f^g·(g'·ln(f) + g·f'/f)"])
go (SinE  e)    = trigDeriv "sin"  e CosE
go (CosE  e)    = trigDeriv "cos"  e (\u -> Sub (Num 0) (SinE u))
go (TanE  e)    = trigDeriv "tan"  e (\u -> Div (Num 1) (Pow (CosE u) (Num 2)))
go (SinhE e)    = trigDeriv "sinh" e CoshE
go (CoshE e)    = trigDeriv "cosh" e SinhE
go (TanhE e)    = trigDeriv "tanh" e (\u -> Div (Num 1) (Pow (CoshE u) (Num 2)))
go (LogE  e)    = trigDeriv "ln"   e (\u -> Div (Num 1) u)
go (ExpE  e)    = trigDeriv "exp"  e ExpE
go (SqrtE e)    = trigDeriv "sqrt" e (\u -> Div (Num 1) (Mul (Num 2) (SqrtE u)))
go (AsinE e)    = trigDeriv "asin" e (\u -> Div (Num 1) (SqrtE (Sub (Num 1) (Pow u (Num 2)))))
go (AcosE e)    = trigDeriv "acos" e (\u -> Div (Sub (Num 0) (Num 1)) (SqrtE (Sub (Num 1) (Pow u (Num 2)))))
go (AtanE e)    = trigDeriv "atan" e (\u -> Div (Num 1) (Add (Num 1) (Pow u (Num 2))))
go (AbsE  e)    = trigDeriv "abs"  e (\u -> Div u (AbsE u))
go (Log2E e)    = trigDeriv "log2" e (\u -> Div (Num 1) (Mul u (LogE (Num 2))))
go (Log10E e)   = trigDeriv "log10" e (\u -> Div (Num 1) (Mul u (LogE (Num 10))))
go (ReE e)      = let (de, ss) = go e in (ReE de, ss ++ ["  Derivative of Re(f) = Re(f')"])
go (ImE e)      = let (de, ss) = go e in (ImE de, ss ++ ["  Derivative of Im(f) = Im(f')"])
go Pi           = (Num 0, ["  Constant: d/dx[π] = 0"])
go E            = (Num 0, ["  Constant: d/dx[e] = 0"])
go Tau          = (Num 0, ["  Constant: d/dx[τ] = 0"])
go Phi          = (Num 0, ["  Constant: d/dx[φ] = 0"])
go I            = (Num 0, ["  Constant: d/dx[i] = 0"])
go Ans          = (Num 0, ["  ans is constant w.r.t. x"])
go CscE{}   = (Num 0, ["  Symbolic deriv of csc not yet implemented"])
go SecE{}   = (Num 0, ["  Symbolic deriv of sec not yet implemented"])
go CotE{}   = (Num 0, ["  Symbolic deriv of cot not yet implemented"])
go AsinhE{} = (Num 0, ["  Symbolic deriv of asinh not yet implemented"])
go AcoshE{} = (Num 0, ["  Symbolic deriv of acosh not yet implemented"])
go AtanhE{} = (Num 0, ["  Symbolic deriv of atanh not yet implemented"])
go AcscE{}  = (Num 0, ["  Symbolic deriv of acsc not yet implemented"])
go AsecE{}  = (Num 0, ["  Symbolic deriv of asec not yet implemented"])
go AcotE{}  = (Num 0, ["  Symbolic deriv of acot not yet implemented"])
go CschE{}  = (Num 0, ["  Symbolic deriv of csch not yet implemented"])
go SechE{}  = (Num 0, ["  Symbolic deriv of sech not yet implemented"])
go CothE{}  = (Num 0, ["  Symbolic deriv of coth not yet implemented"])
go FloorE{} = (Num 0, ["  Derivative of floor is piecewise constant (a.e. 0)"])
go CeilE{}  = (Num 0, ["  Derivative of ceil is piecewise constant (a.e. 0)"])
go RoundE{} = (Num 0, ["  Derivative of round is piecewise constant (a.e. 0)"])
go GammaE{} = (Num 0, ["  Symbolic deriv of Γ requires digamma function"])
go ErfE{}   = (Num 0, ["  Symbolic deriv of erf(x) = 2·exp(-x²)/√π"])
go SignE{}  = (Num 0, ["  Derivative of sign is 0 almost everywhere"])
go ConjE{}  = (Num 0, ["  Derivative of conj uses Wirtinger calculus"])

binDeriv :: String -> Expr -> Expr -> (Expr -> Expr -> Expr) -> (Expr, [String])
binDeriv op l r mk =
    let (dl, sl) = go l
        (dr, sr) = go r
    in (mk dl dr, sl ++ sr ++ ["  " ++ op ++ " rule: (f " ++ op ++ " g)' = f' " ++ op ++ " g'"])

trigDeriv :: String -> Expr -> (Expr -> Expr) -> (Expr, [String])
trigDeriv name e derivF =
    let (de, steps) = go e
    in (Mul (derivF e) de,
        steps ++ ["  Chain rule: d/dx[" ++ name ++ "(u)] = " ++ name ++ "'(u)·u' with u = " ++ showExpr e])

------------------------------------------------------------
-- FORMAT STEPS
------------------------------------------------------------

showDeriveSteps :: Expr -> String
showDeriveSteps expr =
    let (result, steps) = derive expr
    in unlines steps ++ "\n= " ++ showExpr result

------------------------------------------------------------
-- EXPRESSION PRINTER
------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num n)       = showNum n
showExpr Var           = "x"
showExpr Ans           = "ans"
showExpr Pi            = "π"
showExpr E             = "e"
showExpr Tau           = "τ"
showExpr Phi           = "φ"
showExpr I             = "i"
showExpr (Add l r)     = showExpr l ++ " + " ++ showExpr r
showExpr (Sub l r)     = showExpr l ++ " - " ++ showParenS r
showExpr (Mul l r)     = showFactor l ++ "·" ++ showFactor r
showExpr (Div l r)     = showFactor l ++ "/" ++ showFactor r
showExpr (Pow l r)     = showPower l ++ "^" ++ showPower r
showExpr (SinE  e)     = "sin(" ++ showExpr e ++ ")"
showExpr (CosE  e)     = "cos(" ++ showExpr e ++ ")"
showExpr (TanE  e)     = "tan(" ++ showExpr e ++ ")"
showExpr (CscE  e)     = "csc(" ++ showExpr e ++ ")"
showExpr (SecE  e)     = "sec(" ++ showExpr e ++ ")"
showExpr (CotE  e)     = "cot(" ++ showExpr e ++ ")"
showExpr (AsinE e)     = "asin(" ++ showExpr e ++ ")"
showExpr (AcosE e)     = "acos(" ++ showExpr e ++ ")"
showExpr (AtanE e)     = "atan(" ++ showExpr e ++ ")"
showExpr (AcscE e)     = "acsc(" ++ showExpr e ++ ")"
showExpr (AsecE e)     = "asec(" ++ showExpr e ++ ")"
showExpr (AcotE e)     = "acot(" ++ showExpr e ++ ")"
showExpr (SinhE e)     = "sinh(" ++ showExpr e ++ ")"
showExpr (CoshE e)     = "cosh(" ++ showExpr e ++ ")"
showExpr (TanhE e)     = "tanh(" ++ showExpr e ++ ")"
showExpr (CschE e)     = "csch(" ++ showExpr e ++ ")"
showExpr (SechE e)     = "sech(" ++ showExpr e ++ ")"
showExpr (CothE e)     = "coth(" ++ showExpr e ++ ")"
showExpr (AsinhE e)    = "asinh(" ++ showExpr e ++ ")"
showExpr (AcoshE e)    = "acosh(" ++ showExpr e ++ ")"
showExpr (AtanhE e)    = "atanh(" ++ showExpr e ++ ")"
showExpr (LogE  e)     = "ln(" ++ showExpr e ++ ")"
showExpr (Log2E e)     = "log₂(" ++ showExpr e ++ ")"
showExpr (Log10E e)    = "log₁₀(" ++ showExpr e ++ ")"
showExpr (ExpE  e)     = "exp(" ++ showExpr e ++ ")"
showExpr (SqrtE e)     = "√(" ++ showExpr e ++ ")"
showExpr (AbsE  e)     = "|" ++ showExpr e ++ "|"
showExpr (SignE e)     = "sgn(" ++ showExpr e ++ ")"
showExpr (FloorE e)    = "⌊" ++ showExpr e ++ "⌋"
showExpr (CeilE  e)    = "⌈" ++ showExpr e ++ "⌉"
showExpr (RoundE e)    = "round(" ++ showExpr e ++ ")"
showExpr (GammaE e)    = "Γ(" ++ showExpr e ++ ")"
showExpr (ErfE   e)    = "erf(" ++ showExpr e ++ ")"
showExpr (ConjE  e)    = "conj(" ++ showExpr e ++ ")"
showExpr (ReE    e)    = "Re(" ++ showExpr e ++ ")"
showExpr (ImE    e)    = "Im(" ++ showExpr e ++ ")"

showParenS :: Expr -> String
showParenS e@(Add _ _) = "(" ++ showExpr e ++ ")"
showParenS e@(Sub _ _) = "(" ++ showExpr e ++ ")"
showParenS e           = showExpr e

showFactor :: Expr -> String
showFactor e@(Add _ _) = "(" ++ showExpr e ++ ")"
showFactor e@(Sub _ _) = "(" ++ showExpr e ++ ")"
showFactor e           = showExpr e

showPower :: Expr -> String
showPower e@(Add _ _) = "(" ++ showExpr e ++ ")"
showPower e@(Sub _ _) = "(" ++ showExpr e ++ ")"
showPower e@(Mul _ _) = "(" ++ showExpr e ++ ")"
showPower e@(Div _ _) = "(" ++ showExpr e ++ ")"
showPower e@(Pow _ _) = "(" ++ showExpr e ++ ")"
showPower e           = showExpr e

showNum :: Double -> String
showNum n
    | n == fromIntegral (round n :: Int) = show (round n :: Int)
    | otherwise = let s = show n
                      s' = if '.' `elem` s
                           then reverse (dropWhile (== '0') (reverse s))
                           else s
                  in if last s' == '.' then init s' else s'
