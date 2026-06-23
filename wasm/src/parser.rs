use crate::ast::AST;
use crate::token::{Token, tokenize};

fn is_primary_start(toks: &[Token]) -> bool {
    if toks.is_empty() {
        return false;
    }
    matches!(
        &toks[0],
        Token::Num(_)
            | Token::Var
            | Token::Ans
            | Token::LParen
            | Token::Pi
            | Token::E
            | Token::Tau
            | Token::Phi
            | Token::I
            | Token::Sin
            | Token::Cos
            | Token::Tan
            | Token::Csc
            | Token::Sec
            | Token::Cot
            | Token::Asin
            | Token::Acos
            | Token::Atan
            | Token::Acsc
            | Token::Asec
            | Token::Acot
            | Token::Sinh
            | Token::Cosh
            | Token::Tanh
            | Token::Csch
            | Token::Sech
            | Token::Coth
            | Token::Asinh
            | Token::Acosh
            | Token::Atanh
            | Token::Log
            | Token::Log2
            | Token::Log10
            | Token::Log1p
            | Token::Exp
            | Token::Expm1
            | Token::Sqrt
            | Token::Cbrt
            | Token::Abs
            | Token::Sign
            | Token::Floor
            | Token::Ceil
            | Token::Round
            | Token::Gamma
            | Token::Erf
            | Token::Conj
            | Token::Re
            | Token::Im
    )
}

fn parse_compare(toks: &[Token], i: usize) -> Option<(AST, usize)> {
    let (mut left, mut i) = parse_add_sub(toks, i)?;
    loop {
        if i >= toks.len() {
            break;
        }
        match &toks[i] {
            Token::Greater => {
                let (right, j) = parse_add_sub(toks, i + 1)?;
                left = AST::Gt(Box::new(left), Box::new(right));
                i = j;
            }
            Token::GreaterEq => {
                let (right, j) = parse_add_sub(toks, i + 1)?;
                left = AST::Ge(Box::new(left), Box::new(right));
                i = j;
            }
            Token::Less => {
                let (right, j) = parse_add_sub(toks, i + 1)?;
                left = AST::Lt(Box::new(left), Box::new(right));
                i = j;
            }
            Token::LessEq => {
                let (right, j) = parse_add_sub(toks, i + 1)?;
                left = AST::Le(Box::new(left), Box::new(right));
                i = j;
            }
            Token::Eq => {
                let (right, j) = parse_add_sub(toks, i + 1)?;
                left = AST::Eqq(Box::new(left), Box::new(right));
                i = j;
            }
            Token::Neq => {
                let (right, j) = parse_add_sub(toks, i + 1)?;
                left = AST::Neq(Box::new(left), Box::new(right));
                i = j;
            }
            _ => break,
        }
    }
    Some((left, i))
}

fn parse_add_sub(toks: &[Token], i: usize) -> Option<(AST, usize)> {
    let (mut left, mut i) = parse_mul_div(toks, i)?;
    while i < toks.len() {
        match &toks[i] {
            Token::Plus => {
                let (right, j) = parse_mul_div(toks, i + 1)?;
                left = AST::Add(Box::new(left), Box::new(right));
                i = j;
            }
            Token::Minus => {
                let (right, j) = parse_mul_div(toks, i + 1)?;
                left = AST::Sub(Box::new(left), Box::new(right));
                i = j;
            }
            _ => break,
        }
    }
    Some((left, i))
}

fn parse_mul_div(toks: &[Token], i: usize) -> Option<(AST, usize)> {
    let (mut left, mut i) = parse_power(toks, i)?;
    while i < toks.len() {
        match &toks[i] {
            Token::Times => {
                let (right, j) = parse_power(toks, i + 1)?;
                left = AST::Mul(Box::new(left), Box::new(right));
                i = j;
            }
            Token::Divide => {
                let (right, j) = parse_power(toks, i + 1)?;
                left = AST::Div(Box::new(left), Box::new(right));
                i = j;
            }
            _ => {
                if is_primary_start(&toks[i..]) {
                    let (right, j) = parse_power(toks, i)?;
                    left = AST::Mul(Box::new(left), Box::new(right));
                    i = j;
                } else {
                    break;
                }
            }
        }
    }
    Some((left, i))
}

fn parse_power(toks: &[Token], i: usize) -> Option<(AST, usize)> {
    let (left, i) = parse_unary(toks, i)?;
    if i < toks.len() && toks[i] == Token::Power {
        let (right, j) = parse_power(toks, i + 1)?;
        Some((AST::Pow(Box::new(left), Box::new(right)), j))
    } else {
        Some((left, i))
    }
}

fn parse_unary(toks: &[Token], i: usize) -> Option<(AST, usize)> {
    if i < toks.len() {
        if toks[i] == Token::Minus {
            let (a, i) = parse_unary(toks, i + 1)?;
            return Some((AST::Sub(Box::new(AST::Num(0.0)), Box::new(a)), i));
        }
        if toks[i] == Token::Plus {
            return parse_unary(toks, i + 1);
        }
    }
    parse_primary(toks, i)
}

fn parse_primary(toks: &[Token], i: usize) -> Option<(AST, usize)> {
    if i >= toks.len() {
        return None;
    }
    match &toks[i] {
        Token::Num(n) => Some((AST::Num(*n), i + 1)),
        Token::Pi => Some((AST::Pi, i + 1)),
        Token::E => Some((AST::E, i + 1)),
        Token::Tau => Some((AST::Tau, i + 1)),
        Token::Phi => Some((AST::Phi, i + 1)),
        Token::I => Some((AST::I, i + 1)),
        Token::Var => Some((AST::Var, i + 1)),
        Token::Ans => Some((AST::Ans, i + 1)),
        Token::LParen => {
            let (e, j) = parse_compare(toks, i + 1)?;
            if j < toks.len() && toks[j] == Token::RParen {
                Some((e, j + 1))
            } else {
                Some((e, j))
            }
        }
        Token::Sin => parse_fn(toks, i + 1, |a| AST::Sin(Box::new(a))),
        Token::Cos => parse_fn(toks, i + 1, |a| AST::Cos(Box::new(a))),
        Token::Tan => parse_fn(toks, i + 1, |a| AST::Tan(Box::new(a))),
        Token::Csc => parse_fn(toks, i + 1, |a| AST::Csc(Box::new(a))),
        Token::Sec => parse_fn(toks, i + 1, |a| AST::Sec(Box::new(a))),
        Token::Cot => parse_fn(toks, i + 1, |a| AST::Cot(Box::new(a))),
        Token::Asin => parse_fn(toks, i + 1, |a| AST::Asin(Box::new(a))),
        Token::Acos => parse_fn(toks, i + 1, |a| AST::Acos(Box::new(a))),
        Token::Atan => parse_fn(toks, i + 1, |a| AST::Atan(Box::new(a))),
        Token::Acsc => parse_fn(toks, i + 1, |a| AST::Acsc(Box::new(a))),
        Token::Asec => parse_fn(toks, i + 1, |a| AST::Asec(Box::new(a))),
        Token::Acot => parse_fn(toks, i + 1, |a| AST::Acot(Box::new(a))),
        Token::Sinh => parse_fn(toks, i + 1, |a| AST::Sinh(Box::new(a))),
        Token::Cosh => parse_fn(toks, i + 1, |a| AST::Cosh(Box::new(a))),
        Token::Tanh => parse_fn(toks, i + 1, |a| AST::Tanh(Box::new(a))),
        Token::Csch => parse_fn(toks, i + 1, |a| AST::Csch(Box::new(a))),
        Token::Sech => parse_fn(toks, i + 1, |a| AST::Sech(Box::new(a))),
        Token::Coth => parse_fn(toks, i + 1, |a| AST::Coth(Box::new(a))),
        Token::Asinh => parse_fn(toks, i + 1, |a| AST::Asinh(Box::new(a))),
        Token::Acosh => parse_fn(toks, i + 1, |a| AST::Acosh(Box::new(a))),
        Token::Atanh => parse_fn(toks, i + 1, |a| AST::Atanh(Box::new(a))),
        Token::Log => parse_fn(toks, i + 1, |a| AST::Log(Box::new(a))),
        Token::Log2 => parse_fn(toks, i + 1, |a| AST::Log2(Box::new(a))),
        Token::Log10 => parse_fn(toks, i + 1, |a| AST::Log10(Box::new(a))),
        Token::Log1p => parse_fn(toks, i + 1, |a| AST::Log1p(Box::new(a))),
        Token::Exp => parse_fn(toks, i + 1, |a| AST::Exp(Box::new(a))),
        Token::Expm1 => parse_fn(toks, i + 1, |a| AST::Expm1(Box::new(a))),
        Token::Sqrt => parse_fn(toks, i + 1, |a| AST::Sqrt(Box::new(a))),
        Token::Cbrt => parse_fn(toks, i + 1, |a| AST::Cbrt(Box::new(a))),
        Token::Abs => parse_fn(toks, i + 1, |a| AST::Abs(Box::new(a))),
        Token::Sign => parse_fn(toks, i + 1, |a| AST::Sign(Box::new(a))),
        Token::Floor => parse_fn(toks, i + 1, |a| AST::Floor(Box::new(a))),
        Token::Ceil => parse_fn(toks, i + 1, |a| AST::Ceil(Box::new(a))),
        Token::Round => parse_fn(toks, i + 1, |a| AST::Round(Box::new(a))),
        Token::Gamma => parse_fn(toks, i + 1, |a| AST::Gamma(Box::new(a))),
        Token::Erf => parse_fn(toks, i + 1, |a| AST::Erf(Box::new(a))),
        Token::Conj => parse_fn(toks, i + 1, |a| AST::Conj(Box::new(a))),
        Token::Re => parse_fn(toks, i + 1, |a| AST::Re(Box::new(a))),
        Token::Im => parse_fn(toks, i + 1, |a| AST::Im(Box::new(a))),
        _ => None,
    }
}

fn parse_fn(toks: &[Token], i: usize, ctor: impl Fn(AST) -> AST) -> Option<(AST, usize)> {
    if i < toks.len() && toks[i] == Token::LParen {
        let (e, j) = parse_compare(toks, i + 1)?;
        if j < toks.len() && toks[j] == Token::RParen {
            Some((ctor(e), j + 1))
        } else {
            Some((ctor(e), j))
        }
    } else {
        let (e, j) = parse_primary(toks, i)?;
        Some((ctor(e), j))
    }
}

pub fn parse(s: &str) -> Option<AST> {
    let toks = tokenize(s);
    if toks.is_empty() {
        return None;
    }
    parse_compare(&toks, 0).map(|(e, _)| e)
}
