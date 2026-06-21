module Integra.Token (Token(..), lexer) where

import Data.Char (isDigit, isSpace, isAlpha)

data Token
    = NumberTok Double
    | PlusTok | MinusTok | TimesTok | DivideTok | PowerTok
    | LParenTok | RParenTok
    | VarTok
    | SinTok | CosTok | TanTok
    | AsinTok | AcosTok | AtanTok
    | SinhTok | CoshTok | TanhTok
    | LogTok | ExpTok | SqrtTok
    | AbsTok | FloorTok | CeilTok | RoundTok
    | PiTok | ETok
    | AnsTok
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs) | isSpace x = lexer xs
lexer ('+':xs) = PlusTok    : lexer xs
lexer ('-':xs) = MinusTok   : lexer xs
lexer ('*':xs) = TimesTok   : lexer xs
lexer ('/':xs) = DivideTok  : lexer xs
lexer ('^':xs) = PowerTok   : lexer xs
lexer ('(':xs) = LParenTok  : lexer xs
lexer (')':xs) = RParenTok  : lexer xs
lexer (x:xs) | isDigit x || x == '.' =
    let (num, rest) = span (\c -> isDigit c || c == '.') (x:xs)
    in NumberTok (read num) : lexer rest
lexer (x:xs) | isAlpha x =
    let (name, rest) = span isAlpha (x:xs)
    in case name of
        "sin"   -> SinTok   : lexer rest
        "cos"   -> CosTok   : lexer rest
        "tan"   -> TanTok   : lexer rest
        "asin"  -> AsinTok  : lexer rest
        "acos"  -> AcosTok  : lexer rest
        "atan"  -> AtanTok  : lexer rest
        "sinh"  -> SinhTok  : lexer rest
        "cosh"  -> CoshTok  : lexer rest
        "tanh"  -> TanhTok  : lexer rest
        "log"   -> LogTok   : lexer rest
        "exp"   -> ExpTok   : lexer rest
        "sqrt"  -> SqrtTok  : lexer rest
        "abs"   -> AbsTok   : lexer rest
        "floor" -> FloorTok : lexer rest
        "ceil"  -> CeilTok  : lexer rest
        "round" -> RoundTok : lexer rest
        "pi"    -> PiTok    : lexer rest
        "e"     -> ETok     : lexer rest
        "x"     -> VarTok   : lexer rest
        "ans"   -> AnsTok   : lexer rest
        _       -> error ("Unknown identifier: " ++ name)
lexer (x:_) = error ("Unknown character: " ++ [x])
