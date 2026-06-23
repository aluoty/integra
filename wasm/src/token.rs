#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Num(f64),
    Plus,
    Minus,
    Times,
    Divide,
    Power,
    LParen,
    RParen,
    Var,
    Ans,
    Sin,
    Cos,
    Tan,
    Csc,
    Sec,
    Cot,
    Asin,
    Acos,
    Atan,
    Acsc,
    Asec,
    Acot,
    Sinh,
    Cosh,
    Tanh,
    Csch,
    Sech,
    Coth,
    Asinh,
    Acosh,
    Atanh,
    Log,
    Log2,
    Log10,
    Log1p,
    Exp,
    Expm1,
    Sqrt,
    Cbrt,
    Abs,
    Sign,
    Floor,
    Ceil,
    Round,
    Gamma,
    Erf,
    Conj,
    Re,
    Im,
    Pi,
    E,
    Tau,
    Phi,
    I,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Eq,
    Neq,
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let s = s.as_bytes();
    let mut t: Vec<Token> = Vec::new();
    let mut i = 0;
    while i < s.len() {
        if s[i] == b' ' || s[i] == b'\t' {
            i += 1;
            continue;
        }
        if s[i] == b'+' { t.push(Token::Plus); i += 1; continue; }
        if s[i] == b'-' { t.push(Token::Minus); i += 1; continue; }
        if s[i] == b'*' { t.push(Token::Times); i += 1; continue; }
        if s[i] == b'/' { t.push(Token::Divide); i += 1; continue; }
        if s[i] == b'^' { t.push(Token::Power); i += 1; continue; }
        if s[i] == b'(' { t.push(Token::LParen); i += 1; continue; }
        if s[i] == b')' { t.push(Token::RParen); i += 1; continue; }

        if s[i] == b'>' {
            if i + 1 < s.len() && s[i + 1] == b'=' {
                t.push(Token::GreaterEq); i += 2; continue;
            }
            t.push(Token::Greater); i += 1; continue;
        }
        if s[i] == b'<' {
            if i + 1 < s.len() && s[i + 1] == b'=' {
                t.push(Token::LessEq); i += 2; continue;
            }
            t.push(Token::Less); i += 1; continue;
        }
        if s[i] == b'=' && i + 1 < s.len() && s[i + 1] == b'=' {
            t.push(Token::Eq); i += 2; continue;
        }
        if s[i] == b'!' && i + 1 < s.len() && s[i + 1] == b'=' {
            t.push(Token::Neq); i += 2; continue;
        }

        if s[i] == b'.' || (s[i] >= b'0' && s[i] <= b'9') {
            let start = i;
            while i < s.len() && (s[i] >= b'0' && s[i] <= b'9' || s[i] == b'.') {
                i += 1;
            }
            let num_str = std::str::from_utf8(&s[start..i]).unwrap();
            let v: f64 = num_str.parse().unwrap_or(0.0);
            t.push(Token::Num(v));
            continue;
        }

        if (s[i] >= b'a' && s[i] <= b'z') || (s[i] >= b'A' && s[i] <= b'Z') {
            let start = i;
            while i < s.len()
                && ((s[i] >= b'a' && s[i] <= b'z')
                    || (s[i] >= b'A' && s[i] <= b'Z')
                    || (s[i] >= b'0' && s[i] <= b'9'))
            {
                i += 1;
            }
            let id = std::str::from_utf8(&s[start..i]).unwrap();
            match id {
                "sin" => t.push(Token::Sin),
                "cos" => t.push(Token::Cos),
                "tan" => t.push(Token::Tan),
                "csc" => t.push(Token::Csc),
                "sec" => t.push(Token::Sec),
                "cot" => t.push(Token::Cot),
                "asin" => t.push(Token::Asin),
                "acos" => t.push(Token::Acos),
                "atan" => t.push(Token::Atan),
                "acsc" => t.push(Token::Acsc),
                "asec" => t.push(Token::Asec),
                "acot" => t.push(Token::Acot),
                "sinh" => t.push(Token::Sinh),
                "cosh" => t.push(Token::Cosh),
                "tanh" => t.push(Token::Tanh),
                "csch" => t.push(Token::Csch),
                "sech" => t.push(Token::Sech),
                "coth" => t.push(Token::Coth),
                "asinh" => t.push(Token::Asinh),
                "acosh" => t.push(Token::Acosh),
                "atanh" => t.push(Token::Atanh),
                "ln" => t.push(Token::Log),
                "log1p" => t.push(Token::Log1p),
                "log10" => t.push(Token::Log10),
                "log2" => t.push(Token::Log2),
                "log" => t.push(Token::Log),
                "exp" => t.push(Token::Exp),
                "expm1" => t.push(Token::Expm1),
                "sqrt" => t.push(Token::Sqrt),
                "cbrt" => t.push(Token::Cbrt),
                "abs" => t.push(Token::Abs),
                "sign" => t.push(Token::Sign),
                "floor" => t.push(Token::Floor),
                "ceil" => t.push(Token::Ceil),
                "round" => t.push(Token::Round),
                "gamma" => t.push(Token::Gamma),
                "erf" => t.push(Token::Erf),
                "conj" => t.push(Token::Conj),
                "re" => t.push(Token::Re),
                "im" => t.push(Token::Im),
                "pi" => t.push(Token::Pi),
                "tau" => t.push(Token::Tau),
                "phi" => t.push(Token::Phi),
                "inf" => t.push(Token::Num(f64::INFINITY)),
                "e" => t.push(Token::E),
                "i" => t.push(Token::I),
                "x" => t.push(Token::Var),
                "ans" => t.push(Token::Ans),
                _ => {} // ignore unknown identifiers
            }
            continue;
        }

        i += 1;
    }
    t
}
