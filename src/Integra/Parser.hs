module Integra.Parser (parse) where

import Integra.Token (Token(..))
import Integra.AST (Expr(..))

parse :: [Token] -> Expr
parse tokens = case parseAddSub tokens of
    (e, []) -> e
    (_, rest) -> error ("Unexpected tokens: " ++ show rest)

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
    in case rest of
        PowerTok : rest' -> let (r, rest'') = parsePower rest' in (Pow left r, rest'')
        _ -> (left, rest)

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
parsePrimary (LogTok   : LParenTok : rest) = parseFn rest LogE
parsePrimary (Log2Tok  : LParenTok : rest) = parseFn rest Log2E
parsePrimary (Log10Tok : LParenTok : rest) = parseFn rest Log10E
parsePrimary (ExpTok   : LParenTok : rest) = parseFn rest ExpE
parsePrimary (SqrtTok  : LParenTok : rest) = parseFn rest SqrtE
parsePrimary (AbsTok   : LParenTok : rest) = parseFn rest AbsE
parsePrimary (SignTok  : LParenTok : rest) = parseFn rest SignE
parsePrimary (FloorTok : LParenTok : rest) = parseFn rest FloorE
parsePrimary (CeilTok  : LParenTok : rest) = parseFn rest CeilE
parsePrimary (RoundTok : LParenTok : rest) = parseFn rest RoundE
parsePrimary (GammaTok : LParenTok : rest) = parseFn rest GammaE
parsePrimary (ErfTok   : LParenTok : rest) = parseFn rest ErfE

parsePrimary (LParenTok : rest) =
    let (e, rest') = parseAddSub rest
    in case rest' of
        RParenTok : rest'' -> (e, rest'')
        _ -> error "Expected ')'"

parsePrimary tokens =
    error ("Expected expression, got: " ++ show (take 3 tokens))

parseFn :: [Token] -> (Expr -> Expr) -> (Expr, [Token])
parseFn rest ctor =
    let (e, rest') = parseAddSub rest
    in case rest' of
        RParenTok : rest'' -> (ctor e, rest'')
        _ -> error "Expected ')' after function call"
