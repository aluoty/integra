module Integra.Token (Token(..), lexer) where

import Data.Char (isDigit, isSpace, isAlpha)

data Token
    = NumberTok Double
    | PlusTok | MinusTok | TimesTok | DivideTok | PowerTok
    | LParenTok | RParenTok
    | VarTok
    | SinTok | CosTok | TanTok
    | CscTok | SecTok | CotTok
    | AsinTok | AcosTok | AtanTok
    | AcscTok | AsecTok | AcotTok
    | SinhTok | CoshTok | TanhTok
    | CschTok | SechTok | CothTok
    | AsinhTok | AcoshTok | AtanhTok
    | LogTok | Log2Tok | Log10Tok | Log1pTok
    | ExpTok | Expm1Tok | SqrtTok | CbrtTok
    | AbsTok | SignTok
    | FloorTok | CeilTok | RoundTok
    | GammaTok | ErfTok
    | ConjTok | ReTok | ImTok
    | PiTok | ETok | TauTok | PhiTok | ITok
    | AnsTok
    | GreaterTok | GreaterEqTok | LessTok | LessEqTok | EqTok | NeqTok
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs) | isSpace x = lexer xs
lexer ('+':xs) = PlusTok       : lexer xs
lexer ('-':xs) = MinusTok      : lexer xs
lexer ('*':xs) = TimesTok      : lexer xs
lexer ('/':xs) = DivideTok     : lexer xs
lexer ('^':xs) = PowerTok      : lexer xs
lexer ('(':xs) = LParenTok     : lexer xs
lexer (')':xs) = RParenTok     : lexer xs
lexer ('>':'=':xs) = GreaterEqTok : lexer xs
lexer ('>':xs)     = GreaterTok    : lexer xs
lexer ('<':'=':xs) = LessEqTok    : lexer xs
lexer ('<':xs)     = LessTok       : lexer xs
lexer ('=':'=':xs) = EqTok         : lexer xs
lexer ('!':'=':xs) = NeqTok        : lexer xs
lexer ('l':'o':'g':'1':'0':xs) = Log10Tok : lexer xs
lexer ('l':'o':'g':'1':'p':xs) = Log1pTok : lexer xs
lexer ('l':'o':'g':'2':xs)     = Log2Tok  : lexer xs
lexer ('e':'x':'p':'m':'1':xs) = Expm1Tok : lexer xs
lexer (x:xs) | isDigit x || x == '.' =
    let (num, rest) = span (\c -> isDigit c || c == '.') (x:xs)
    in NumberTok (read num) : lexer rest
lexer (x:xs) | isAlpha x =
    let (name, rest) = span isAlpha (x:xs)
    in case name of
        "sin"   -> SinTok   : lexer rest
        "cos"   -> CosTok   : lexer rest
        "tan"   -> TanTok   : lexer rest
        "csc"   -> CscTok   : lexer rest
        "sec"   -> SecTok   : lexer rest
        "cot"   -> CotTok   : lexer rest
        "asin"  -> AsinTok  : lexer rest
        "acos"  -> AcosTok  : lexer rest
        "atan"  -> AtanTok  : lexer rest
        "acsc"  -> AcscTok  : lexer rest
        "asec"  -> AsecTok  : lexer rest
        "acot"  -> AcotTok  : lexer rest
        "sinh"  -> SinhTok  : lexer rest
        "cosh"  -> CoshTok  : lexer rest
        "tanh"  -> TanhTok  : lexer rest
        "csch"  -> CschTok  : lexer rest
        "sech"  -> SechTok  : lexer rest
        "coth"  -> CothTok  : lexer rest
        "asinh" -> AsinhTok : lexer rest
        "acosh" -> AcoshTok : lexer rest
        "atanh" -> AtanhTok : lexer rest
        "ln"    -> LogTok   : lexer rest
        "log"   -> LogTok   : lexer rest
        "log1p" -> Log1pTok : lexer rest
        "exp"   -> ExpTok   : lexer rest
        "expm1" -> Expm1Tok : lexer rest
        "sqrt"  -> SqrtTok  : lexer rest
        "cbrt"  -> CbrtTok  : lexer rest
        "abs"   -> AbsTok   : lexer rest
        "sign"  -> SignTok  : lexer rest
        "floor" -> FloorTok : lexer rest
        "ceil"  -> CeilTok  : lexer rest
        "round" -> RoundTok : lexer rest
        "gamma" -> GammaTok : lexer rest
        "erf"   -> ErfTok   : lexer rest
        "conj"  -> ConjTok  : lexer rest
        "re"    -> ReTok    : lexer rest
        "im"    -> ImTok    : lexer rest
        "pi"    -> PiTok    : lexer rest
        "tau"   -> TauTok   : lexer rest
        "phi"   -> PhiTok   : lexer rest
        "e"     -> ETok     : lexer rest
        "inf"   -> NumberTok (1/0) : lexer rest
        "i"     -> ITok     : lexer rest
        "x"     -> VarTok   : lexer rest
        "ans"   -> AnsTok   : lexer rest
        _       -> error ("Unknown identifier: " ++ name)
lexer (x:_) = error ("Unknown character: " ++ [x])
