module Integra.Parser (parse) where

import Integra.Token (Token(..))
import Integra.AST (Expr(..))

parse :: [Token] -> Expr
parse tokens = case parseCompare tokens of
    (e, []) -> e
    (_, rest) -> error ("Unexpected tokens: " ++ show rest)

-- Comparison: lowest precedence (left-assoc)
parseCompare :: [Token] -> (Expr, [Token])
parseCompare tokens =
    case parseAddSub tokens of
        (left, rest) -> go left rest
  where
    go acc (GreaterTok   : rest) = let (r, rest') = parseAddSub rest in go (Gt  acc r) rest'
    go acc (GreaterEqTok : rest) = let (r, rest') = parseAddSub rest in go (Ge  acc r) rest'
    go acc (LessTok      : rest) = let (r, rest') = parseAddSub rest in go (Lt  acc r) rest'
    go acc (LessEqTok    : rest) = let (r, rest') = parseAddSub rest in go (Le  acc r) rest'
    go acc (EqTok        : rest) = let (r, rest') = parseAddSub rest in go (Eqq acc r) rest'
    go acc (NeqTok       : rest) = let (r, rest') = parseAddSub rest in go (Neq acc r) rest'
    go acc rest                  = (acc, rest)

parseAddSub :: [Token] -> (Expr, [Token])
parseAddSub tokens =
    case parseMulDiv tokens of
        (left, rest) -> go left rest
  where
    go acc (PlusTok  : rest) = let (r, rest') = parseMulDiv rest in go (Add acc r) rest'
    go acc (MinusTok : rest) = let (r, rest') = parseMulDiv rest in go (Sub acc r) rest'
    go acc rest              = (acc, rest)

parseMulDiv :: [Token] -> (Expr, [Token])
parseMulDiv tokens =
    case parsePower tokens of
        (left, rest) -> go left rest
  where
    go acc (TimesTok  : rest) = let (r, rest') = parsePower rest in go (Mul acc r) rest'
    go acc (DivideTok : rest) = let (r, rest') = parsePower rest in go (Div acc r) rest'
    go acc rest               = (acc, rest)

parsePower :: [Token] -> (Expr, [Token])
parsePower tokens =
    let (left, rest) = parseUnary tokens
        (base, rest1) = case rest of
            PowerTok : rest' -> let (r, rest'') = parsePower rest' in (Pow left r, rest'')
            _ -> (left, rest)
    in if not (null rest1) && isPrimaryStart rest1
       then implicitMul base rest1
       else (base, rest1)

implicitMul :: Expr -> [Token] -> (Expr, [Token])
implicitMul left rest =
    let (right, rest') = parsePower rest
    in if not (null rest') && isPrimaryStart rest'
       then implicitMul (Mul left right) rest'
       else (Mul left right, rest')

isPrimaryStart :: [Token] -> Bool
isPrimaryStart (NumberTok _ : _) = True
isPrimaryStart (ITok : _)        = True
isPrimaryStart (VarTok : _)      = True
isPrimaryStart (AnsTok : _)      = True
isPrimaryStart (LParenTok : _)   = True
isPrimaryStart (SinTok : _)      = True
isPrimaryStart (CosTok : _)      = True
isPrimaryStart (TanTok : _)      = True
isPrimaryStart (CscTok : _)      = True
isPrimaryStart (SecTok : _)      = True
isPrimaryStart (CotTok : _)      = True
isPrimaryStart (AsinTok : _)     = True
isPrimaryStart (AcosTok : _)     = True
isPrimaryStart (AtanTok : _)     = True
isPrimaryStart (AcscTok : _)     = True
isPrimaryStart (AsecTok : _)     = True
isPrimaryStart (AcotTok : _)     = True
isPrimaryStart (SinhTok : _)     = True
isPrimaryStart (CoshTok : _)     = True
isPrimaryStart (TanhTok : _)     = True
isPrimaryStart (CschTok : _)     = True
isPrimaryStart (SechTok : _)     = True
isPrimaryStart (CothTok : _)     = True
isPrimaryStart (AsinhTok : _)    = True
isPrimaryStart (AcoshTok : _)    = True
isPrimaryStart (AtanhTok : _)    = True
isPrimaryStart (LogTok : _)      = True
isPrimaryStart (Log2Tok : _)     = True
isPrimaryStart (Log10Tok : _)    = True
isPrimaryStart (Log1pTok : _)    = True
isPrimaryStart (ExpTok : _)      = True
isPrimaryStart (Expm1Tok : _)    = True
isPrimaryStart (SqrtTok : _)     = True
isPrimaryStart (CbrtTok : _)     = True
isPrimaryStart (AbsTok : _)      = True
isPrimaryStart (SignTok : _)     = True
isPrimaryStart (FloorTok : _)    = True
isPrimaryStart (CeilTok : _)     = True
isPrimaryStart (RoundTok : _)    = True
isPrimaryStart (GammaTok : _)    = True
isPrimaryStart (ErfTok : _)      = True
isPrimaryStart (ConjTok : _)     = True
isPrimaryStart (ReTok : _)       = True
isPrimaryStart (ImTok : _)       = True
isPrimaryStart (PiTok : _)       = True
isPrimaryStart (ETok : _)        = True
isPrimaryStart (TauTok : _)      = True
isPrimaryStart (PhiTok : _)      = True
isPrimaryStart _                 = False

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (MinusTok : rest) =
    let (e, rest') = parseUnary rest
    in (Sub (Num 0) e, rest')
parseUnary tokens = parsePrimary tokens

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (NumberTok n : rest) = (Num n, rest)
parsePrimary (PiTok    : rest)    = (Pi, rest)
parsePrimary (ETok     : rest)    = (E, rest)
parsePrimary (TauTok   : rest)    = (Tau, rest)
parsePrimary (PhiTok   : rest)    = (Phi, rest)
parsePrimary (ITok     : rest)    = (I, rest)
parsePrimary (VarTok   : rest)    = (Var, rest)
parsePrimary (AnsTok   : rest)    = (Ans, rest)

parsePrimary (SinTok   : LParenTok : rest) = parseFn rest SinE
parsePrimary (CosTok   : LParenTok : rest) = parseFn rest CosE
parsePrimary (TanTok   : LParenTok : rest) = parseFn rest TanE
parsePrimary (CscTok   : LParenTok : rest) = parseFn rest CscE
parsePrimary (SecTok   : LParenTok : rest) = parseFn rest SecE
parsePrimary (CotTok   : LParenTok : rest) = parseFn rest CotE
parsePrimary (AsinTok  : LParenTok : rest) = parseFn rest AsinE
parsePrimary (AcosTok  : LParenTok : rest) = parseFn rest AcosE
parsePrimary (AtanTok  : LParenTok : rest) = parseFn rest AtanE
parsePrimary (AcscTok  : LParenTok : rest) = parseFn rest AcscE
parsePrimary (AsecTok  : LParenTok : rest) = parseFn rest AsecE
parsePrimary (AcotTok  : LParenTok : rest) = parseFn rest AcotE
parsePrimary (SinhTok  : LParenTok : rest) = parseFn rest SinhE
parsePrimary (CoshTok  : LParenTok : rest) = parseFn rest CoshE
parsePrimary (TanhTok  : LParenTok : rest) = parseFn rest TanhE
parsePrimary (CschTok  : LParenTok : rest) = parseFn rest CschE
parsePrimary (SechTok  : LParenTok : rest) = parseFn rest SechE
parsePrimary (CothTok  : LParenTok : rest) = parseFn rest CothE
parsePrimary (AsinhTok : LParenTok : rest) = parseFn rest AsinhE
parsePrimary (AcoshTok : LParenTok : rest) = parseFn rest AcoshE
parsePrimary (AtanhTok : LParenTok : rest) = parseFn rest AtanhE
parsePrimary (LogTok   : LParenTok : rest) = parseFn rest LogE
parsePrimary (Log2Tok  : LParenTok : rest) = parseFn rest Log2E
parsePrimary (Log10Tok : LParenTok : rest) = parseFn rest Log10E
parsePrimary (Log1pTok : LParenTok : rest) = parseFn rest Log1pE
parsePrimary (ExpTok   : LParenTok : rest) = parseFn rest ExpE
parsePrimary (Expm1Tok : LParenTok : rest) = parseFn rest Expm1E
parsePrimary (SqrtTok  : LParenTok : rest) = parseFn rest SqrtE
parsePrimary (CbrtTok  : LParenTok : rest) = parseFn rest CbrtE
parsePrimary (AbsTok   : LParenTok : rest) = parseFn rest AbsE
parsePrimary (SignTok  : LParenTok : rest) = parseFn rest SignE
parsePrimary (FloorTok : LParenTok : rest) = parseFn rest FloorE
parsePrimary (CeilTok  : LParenTok : rest) = parseFn rest CeilE
parsePrimary (RoundTok : LParenTok : rest) = parseFn rest RoundE
parsePrimary (GammaTok : LParenTok : rest) = parseFn rest GammaE
parsePrimary (ErfTok   : LParenTok : rest) = parseFn rest ErfE
parsePrimary (ConjTok  : LParenTok : rest) = parseFn rest ConjE
parsePrimary (ReTok    : LParenTok : rest) = parseFn rest ReE
parsePrimary (ImTok    : LParenTok : rest) = parseFn rest ImE

parsePrimary (LParenTok : rest) =
    let (e, rest') = parseCompare rest
    in case rest' of
        RParenTok : rest'' -> (e, rest'')
        _ -> error "Expected ')'"

parsePrimary tokens =
    error ("Expected expression, got: " ++ show (take 3 tokens))

parseFn :: [Token] -> (Expr -> Expr) -> (Expr, [Token])
parseFn rest ctor =
    let (e, rest') = parseCompare rest
    in case rest' of
        RParenTok : rest'' -> (ctor e, rest'')
        _ -> error "Expected ')' after function call"
