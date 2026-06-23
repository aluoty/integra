use crate::ast::AST;

fn has_var(ast: &AST) -> bool {
    match ast {
        AST::Var => true,
        AST::Ans => false,
        AST::Num(_) => false,
        AST::Pi => false,
        AST::E => false,
        AST::Tau => false,
        AST::Phi => false,
        AST::I => false,
        AST::Add(a, b) | AST::Sub(a, b) | AST::Mul(a, b) | AST::Div(a, b) | AST::Pow(a, b)
        | AST::Gt(a, b) | AST::Ge(a, b) | AST::Lt(a, b) | AST::Le(a, b) | AST::Eqq(a, b)
        | AST::Neq(a, b) => has_var(a) || has_var(b),
        AST::Sin(a) | AST::Cos(a) | AST::Tan(a) | AST::Csc(a) | AST::Sec(a) | AST::Cot(a)
        | AST::Asin(a) | AST::Acos(a) | AST::Atan(a) | AST::Acsc(a) | AST::Asec(a) | AST::Acot(a)
        | AST::Sinh(a) | AST::Cosh(a) | AST::Tanh(a) | AST::Csch(a) | AST::Sech(a) | AST::Coth(a)
        | AST::Asinh(a) | AST::Acosh(a) | AST::Atanh(a) | AST::Log(a) | AST::Log2(a)
        | AST::Log10(a) | AST::Log1p(a) | AST::Exp(a) | AST::Expm1(a) | AST::Sqrt(a) | AST::Cbrt(a)
        | AST::Abs(a) | AST::Sign(a) | AST::Floor(a) | AST::Ceil(a) | AST::Round(a) | AST::Gamma(a)
        | AST::Erf(a) | AST::Conj(a) | AST::Re(a) | AST::Im(a) => has_var(a),
    }
}

fn deriv_raw(ast: &AST) -> AST {
    match ast {
        AST::Num(_) => AST::Num(0.0),
        AST::Pi => AST::Num(0.0),
        AST::E => AST::Num(0.0),
        AST::Tau => AST::Num(0.0),
        AST::Phi => AST::Num(0.0),
        AST::I => AST::Num(0.0),
        AST::Var => AST::Num(1.0),
        AST::Ans => AST::Num(0.0),
        AST::Add(a, b) => AST::Add(Box::new(deriv_raw(a)), Box::new(deriv_raw(b))),
        AST::Sub(a, b) => AST::Sub(Box::new(deriv_raw(a)), Box::new(deriv_raw(b))),
        AST::Mul(a, b) => AST::Add(
            Box::new(AST::Mul(Box::new(deriv_raw(a)), b.clone())),
            Box::new(AST::Mul(a.clone(), Box::new(deriv_raw(b)))),
        ),
        AST::Div(a, b) => AST::Div(
            Box::new(AST::Sub(
                Box::new(AST::Mul(Box::new(deriv_raw(a)), b.clone())),
                Box::new(AST::Mul(a.clone(), Box::new(deriv_raw(b)))),
            )),
            Box::new(AST::Pow(b.clone(), Box::new(AST::Num(2.0)))),
        ),
        AST::Pow(a, b) => {
            if let AST::Num(n) = b.as_ref() {
                AST::Mul(
                    Box::new(AST::Mul(
                        Box::new(AST::Num(*n)),
                        Box::new(AST::Pow(a.clone(), Box::new(AST::Num(n - 1.0)))),
                    )),
                    Box::new(deriv_raw(a)),
                )
            } else {
                AST::Mul(
                    Box::new(AST::Pow(a.clone(), b.clone())),
                    Box::new(deriv_raw(&AST::Mul(
                        b.clone(),
                        Box::new(AST::Log(a.clone())),
                    ))),
                )
            }
        }
        AST::Sin(a) => AST::Mul(Box::new(AST::Cos(a.clone())), Box::new(deriv_raw(a))),
        AST::Cos(a) => AST::Mul(
            Box::new(AST::Sub(Box::new(AST::Num(0.0)), Box::new(AST::Sin(a.clone())))),
            Box::new(deriv_raw(a)),
        ),
        AST::Tan(a) => AST::Mul(
            Box::new(AST::Add(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(Box::new(AST::Tan(a.clone())), Box::new(AST::Num(2.0)))),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Csc(a) => AST::Mul(
            Box::new(AST::Sub(
                Box::new(AST::Num(0.0)),
                Box::new(AST::Mul(Box::new(AST::Csc(a.clone())), Box::new(AST::Cot(a.clone())))),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Sec(a) => AST::Mul(
            Box::new(AST::Mul(Box::new(AST::Sec(a.clone())), Box::new(AST::Tan(a.clone())))),
            Box::new(deriv_raw(a)),
        ),
        AST::Cot(a) => AST::Mul(
            Box::new(AST::Sub(
                Box::new(AST::Num(0.0)),
                Box::new(AST::Add(
                    Box::new(AST::Num(1.0)),
                    Box::new(AST::Pow(Box::new(AST::Cot(a.clone())), Box::new(AST::Num(2.0)))),
                )),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Asin(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Sqrt(Box::new(AST::Sub(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
            )))),
        ),
        AST::Acos(a) => AST::Div(
            Box::new(AST::Sub(Box::new(AST::Num(0.0)), Box::new(deriv_raw(a)))),
            Box::new(AST::Sqrt(Box::new(AST::Sub(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
            )))),
        ),
        AST::Atan(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Add(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
            )),
        ),
        AST::Acsc(a) => AST::Div(
            Box::new(AST::Sub(Box::new(AST::Num(0.0)), Box::new(deriv_raw(a)))),
            Box::new(AST::Mul(
                Box::new(AST::Abs(a.clone())),
                Box::new(AST::Sqrt(Box::new(AST::Sub(
                    Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
                    Box::new(AST::Num(1.0)),
                )))),
            )),
        ),
        AST::Asec(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Mul(
                Box::new(AST::Abs(a.clone())),
                Box::new(AST::Sqrt(Box::new(AST::Sub(
                    Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
                    Box::new(AST::Num(1.0)),
                )))),
            )),
        ),
        AST::Acot(a) => AST::Div(
            Box::new(AST::Sub(Box::new(AST::Num(0.0)), Box::new(deriv_raw(a)))),
            Box::new(AST::Add(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
            )),
        ),
        AST::Sinh(a) => AST::Mul(Box::new(AST::Cosh(a.clone())), Box::new(deriv_raw(a))),
        AST::Cosh(a) => AST::Mul(Box::new(AST::Sinh(a.clone())), Box::new(deriv_raw(a))),
        AST::Tanh(a) => AST::Mul(
            Box::new(AST::Sub(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(Box::new(AST::Tanh(a.clone())), Box::new(AST::Num(2.0)))),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Csch(a) => AST::Mul(
            Box::new(AST::Sub(
                Box::new(AST::Num(0.0)),
                Box::new(AST::Mul(Box::new(AST::Csch(a.clone())), Box::new(AST::Coth(a.clone())))),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Sech(a) => AST::Mul(
            Box::new(AST::Sub(
                Box::new(AST::Num(0.0)),
                Box::new(AST::Mul(Box::new(AST::Sech(a.clone())), Box::new(AST::Tanh(a.clone())))),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Coth(a) => AST::Mul(
            Box::new(AST::Sub(
                Box::new(AST::Num(0.0)),
                Box::new(AST::Sub(
                    Box::new(AST::Num(1.0)),
                    Box::new(AST::Pow(Box::new(AST::Coth(a.clone())), Box::new(AST::Num(2.0)))),
                )),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Asinh(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Sqrt(Box::new(AST::Add(
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
                Box::new(AST::Num(1.0)),
            )))),
        ),
        AST::Acosh(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Sqrt(Box::new(AST::Sub(
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
                Box::new(AST::Num(1.0)),
            )))),
        ),
        AST::Atanh(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Sub(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
            )),
        ),
        AST::Log(a) => AST::Div(Box::new(deriv_raw(a)), a.clone()),
        AST::Log2(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Mul(a.clone(), Box::new(AST::Log(Box::new(AST::Num(2.0)))))),
        ),
        AST::Log10(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Mul(a.clone(), Box::new(AST::Log(Box::new(AST::Num(10.0)))))),
        ),
        AST::Log1p(a) => AST::Div(Box::new(deriv_raw(a)), Box::new(AST::Add(Box::new(AST::Num(1.0)), a.clone()))),
        AST::Exp(a) => AST::Mul(Box::new(AST::Exp(a.clone())), Box::new(deriv_raw(a))),
        AST::Expm1(a) => AST::Mul(Box::new(AST::Exp(a.clone())), Box::new(deriv_raw(a))),
        AST::Sqrt(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Mul(Box::new(AST::Num(2.0)), Box::new(AST::Sqrt(a.clone())))),
        ),
        AST::Cbrt(a) => AST::Div(
            Box::new(deriv_raw(a)),
            Box::new(AST::Mul(
                Box::new(AST::Num(3.0)),
                Box::new(AST::Pow(Box::new(AST::Cbrt(a.clone())), Box::new(AST::Num(2.0)))),
            )),
        ),
        AST::Abs(a) => AST::Mul(
            Box::new(AST::Div(a.clone(), Box::new(AST::Abs(a.clone())))),
            Box::new(deriv_raw(a)),
        ),
        AST::Sign(_) => AST::Num(0.0),
        AST::Floor(_) => AST::Num(0.0),
        AST::Ceil(_) => AST::Num(0.0),
        AST::Round(_) => AST::Num(0.0),
        AST::Gamma(a) => AST::Mul(Box::new(AST::Gamma(a.clone())), Box::new(AST::Num(0.0))),
        AST::Erf(a) => AST::Mul(
            Box::new(AST::Div(
                Box::new(AST::Num(2.0)),
                Box::new(AST::Mul(
                    Box::new(AST::Sqrt(Box::new(AST::Pi))),
                    Box::new(AST::Exp(Box::new(AST::Sub(
                        Box::new(AST::Num(0.0)),
                        Box::new(AST::Pow(a.clone(), Box::new(AST::Num(2.0)))),
                    )))),
                )),
            )),
            Box::new(deriv_raw(a)),
        ),
        AST::Conj(a) => AST::Conj(Box::new(deriv_raw(a))),
        AST::Re(a) => AST::Re(Box::new(deriv_raw(a))),
        AST::Im(a) => AST::Im(Box::new(deriv_raw(a))),
        AST::Gt(_, _) => AST::Num(0.0),
        AST::Ge(_, _) => AST::Num(0.0),
        AST::Lt(_, _) => AST::Num(0.0),
        AST::Le(_, _) => AST::Num(0.0),
        AST::Eqq(_, _) => AST::Num(0.0),
        AST::Neq(_, _) => AST::Num(0.0),
    }
}

fn has_num_zero(ast: &AST) -> bool {
    matches!(ast, AST::Num(n) if *n == 0.0)
}

fn has_num_one(ast: &AST) -> bool {
    matches!(ast, AST::Num(n) if *n == 1.0)
}

fn simplify(ast: AST) -> AST {
    match ast {
        AST::Add(a, b) => {
            let sa = simplify(*a);
            let sb = simplify(*b);
            if has_num_zero(&sa) { return sb; }
            if has_num_zero(&sb) { return sa; }
            if let AST::Num(x) = &sa {
                if let AST::Num(y) = &sb {
                    return AST::Num(x + y);
                }
            }
            AST::Add(Box::new(sa), Box::new(sb))
        }
        AST::Sub(a, b) => {
            let sa = simplify(*a);
            let sb = simplify(*b);
            if has_num_zero(&sb) { return sa; }
            if has_num_zero(&sa) {
                if let AST::Num(n) = &sb {
                    return AST::Num(-n);
                }
                return AST::Mul(Box::new(AST::Num(-1.0)), Box::new(sb));
            }
            if let AST::Num(x) = &sa {
                if let AST::Num(y) = &sb {
                    return AST::Num(x - y);
                }
            }
            AST::Sub(Box::new(sa), Box::new(sb))
        }
        AST::Mul(a, b) => {
            let sa = simplify(*a);
            let sb = simplify(*b);
            if has_num_zero(&sa) || has_num_zero(&sb) { return AST::Num(0.0); }
            if has_num_one(&sa) { return sb; }
            if has_num_one(&sb) { return sa; }
            if let AST::Num(x) = &sa {
                if let AST::Num(y) = &sb {
                    return AST::Num(x * y);
                }
                if let AST::Mul(c, d) = &sb {
                    if let AST::Num(c_val) = c.as_ref() {
                        return simplify(AST::Mul(Box::new(AST::Num(x * c_val)), d.clone()));
                    }
                }
            }
            if let AST::Num(sb_val) = &sb {
                if let AST::Mul(c, d) = &sa {
                    if let AST::Num(c_val) = c.as_ref() {
                        return simplify(AST::Mul(Box::new(AST::Num(sb_val * c_val)), d.clone()));
                    }
                }
            }
            AST::Mul(Box::new(sa), Box::new(sb))
        }
        AST::Div(a, b) => {
            let sa = simplify(*a);
            let sb = simplify(*b);
            if has_num_zero(&sa) { return AST::Num(0.0); }
            if has_num_one(&sb) { return sa; }
            if let AST::Num(x) = &sa {
                if let AST::Num(y) = &sb {
                    return AST::Num(x / y);
                }
            }
            AST::Div(Box::new(sa), Box::new(sb))
        }
        AST::Pow(a, b) => {
            let sa = simplify(*a);
            let sb = simplify(*b);
            if has_num_zero(&sb) { return AST::Num(1.0); }
            if has_num_one(&sb) { return sa; }
            if let AST::Num(x) = &sa {
                if let AST::Num(y) = &sb {
                    return AST::Num(x.powf(*y));
                }
            }
            AST::Pow(Box::new(sa), Box::new(sb))
        }
        AST::Sin(a) => AST::Sin(Box::new(simplify(*a))),
        AST::Cos(a) => AST::Cos(Box::new(simplify(*a))),
        AST::Tan(a) => AST::Tan(Box::new(simplify(*a))),
        AST::Csc(a) => AST::Csc(Box::new(simplify(*a))),
        AST::Sec(a) => AST::Sec(Box::new(simplify(*a))),
        AST::Cot(a) => AST::Cot(Box::new(simplify(*a))),
        AST::Asin(a) => AST::Asin(Box::new(simplify(*a))),
        AST::Acos(a) => AST::Acos(Box::new(simplify(*a))),
        AST::Atan(a) => AST::Atan(Box::new(simplify(*a))),
        AST::Sinh(a) => AST::Sinh(Box::new(simplify(*a))),
        AST::Cosh(a) => AST::Cosh(Box::new(simplify(*a))),
        AST::Tanh(a) => AST::Tanh(Box::new(simplify(*a))),
        AST::Log(ref a) | AST::Log2(ref a) | AST::Log10(ref a) | AST::Log1p(ref a) | AST::Exp(ref a) | AST::Expm1(ref a)
        | AST::Sqrt(ref a) | AST::Cbrt(ref a) | AST::Abs(ref a) | AST::Sign(ref a) | AST::Floor(ref a) | AST::Ceil(ref a)
        | AST::Round(ref a) | AST::Gamma(ref a) | AST::Erf(ref a) | AST::Conj(ref a) | AST::Re(ref a) | AST::Im(ref a) => {
            let inner = simplify(*a.clone());
            match ast {
                AST::Log(_) => AST::Log(Box::new(inner)),
                AST::Log2(_) => AST::Log2(Box::new(inner)),
                AST::Log10(_) => AST::Log10(Box::new(inner)),
                AST::Log1p(_) => AST::Log1p(Box::new(inner)),
                AST::Exp(_) => AST::Exp(Box::new(inner)),
                AST::Expm1(_) => AST::Expm1(Box::new(inner)),
                AST::Sqrt(_) => AST::Sqrt(Box::new(inner)),
                AST::Cbrt(_) => AST::Cbrt(Box::new(inner)),
                AST::Abs(_) => AST::Abs(Box::new(inner)),
                AST::Sign(_) => AST::Sign(Box::new(inner)),
                AST::Floor(_) => AST::Floor(Box::new(inner)),
                AST::Ceil(_) => AST::Ceil(Box::new(inner)),
                AST::Round(_) => AST::Round(Box::new(inner)),
                AST::Gamma(_) => AST::Gamma(Box::new(inner)),
                AST::Erf(_) => AST::Erf(Box::new(inner)),
                AST::Conj(_) => AST::Conj(Box::new(inner)),
                AST::Re(_) => AST::Re(Box::new(inner)),
                AST::Im(_) => AST::Im(Box::new(inner)),
                _ => unreachable!(),
            }
        }
        _ => ast,
    }
}

pub fn deriv(ast: &AST) -> AST {
    simplify(deriv_raw(ast))
}

pub fn show_ast(ast: &AST) -> String {
    match ast {
        AST::Num(n) => {
            if n.fract() == 0.0 && n.abs() < 1e15 {
                format!("{}", *n as i64)
            } else {
                let s = format!("{:.10}", n);
                let s = s.trim_end_matches('0');
                let s = s.trim_end_matches('.');
                s.to_string()
            }
        }
        AST::Pi => "π".into(),
        AST::E => "e".into(),
        AST::Tau => "τ".into(),
        AST::Phi => "φ".into(),
        AST::I => "i".into(),
        AST::Var => "x".into(),
        AST::Ans => "ans".into(),
        AST::Add(a, b) => format!("{} + {}", show_ast(a), show_ast(b)),
        AST::Sub(a, b) => format!("{} - {}", show_ast(a), show_paren(b)),
        AST::Mul(a, b) => format!("{}{}", show_factor(a), show_factor(b)),
        AST::Div(a, b) => format!("{}/{}", show_ast(a), show_paren(b)),
        AST::Pow(a, b) => format!("{}^{}", show_factor(a), show_factor(b)),
        AST::Gt(a, b) => format!("{} > {}", show_ast(a), show_ast(b)),
        AST::Ge(a, b) => format!("{} >= {}", show_ast(a), show_ast(b)),
        AST::Lt(a, b) => format!("{} < {}", show_ast(a), show_ast(b)),
        AST::Le(a, b) => format!("{} <= {}", show_ast(a), show_ast(b)),
        AST::Eqq(a, b) => format!("{} == {}", show_ast(a), show_ast(b)),
        AST::Neq(a, b) => format!("{} != {}", show_ast(a), show_ast(b)),
        AST::Sin(a) => format!("sin({})", show_ast(a)),
        AST::Cos(a) => format!("cos({})", show_ast(a)),
        AST::Tan(a) => format!("tan({})", show_ast(a)),
        AST::Csc(a) => format!("csc({})", show_ast(a)),
        AST::Sec(a) => format!("sec({})", show_ast(a)),
        AST::Cot(a) => format!("cot({})", show_ast(a)),
        AST::Asin(a) => format!("asin({})", show_ast(a)),
        AST::Acos(a) => format!("acos({})", show_ast(a)),
        AST::Atan(a) => format!("atan({})", show_ast(a)),
        AST::Acsc(a) => format!("acsc({})", show_ast(a)),
        AST::Asec(a) => format!("asec({})", show_ast(a)),
        AST::Acot(a) => format!("acot({})", show_ast(a)),
        AST::Sinh(a) => format!("sinh({})", show_ast(a)),
        AST::Cosh(a) => format!("cosh({})", show_ast(a)),
        AST::Tanh(a) => format!("tanh({})", show_ast(a)),
        AST::Csch(a) => format!("csch({})", show_ast(a)),
        AST::Sech(a) => format!("sech({})", show_ast(a)),
        AST::Coth(a) => format!("coth({})", show_ast(a)),
        AST::Asinh(a) => format!("asinh({})", show_ast(a)),
        AST::Acosh(a) => format!("acosh({})", show_ast(a)),
        AST::Atanh(a) => format!("atanh({})", show_ast(a)),
        AST::Log(a) => format!("ln({})", show_ast(a)),
        AST::Log2(a) => format!("log2({})", show_ast(a)),
        AST::Log10(a) => format!("log10({})", show_ast(a)),
        AST::Log1p(a) => format!("log1p({})", show_ast(a)),
        AST::Exp(a) => format!("exp({})", show_ast(a)),
        AST::Expm1(a) => format!("expm1({})", show_ast(a)),
        AST::Sqrt(a) => format!("sqrt({})", show_ast(a)),
        AST::Cbrt(a) => format!("cbrt({})", show_ast(a)),
        AST::Abs(a) => format!("abs({})", show_ast(a)),
        AST::Sign(a) => format!("sign({})", show_ast(a)),
        AST::Floor(a) => format!("floor({})", show_ast(a)),
        AST::Ceil(a) => format!("ceil({})", show_ast(a)),
        AST::Round(a) => format!("round({})", show_ast(a)),
        AST::Gamma(a) => format!("Γ({})", show_ast(a)),
        AST::Erf(a) => format!("erf({})", show_ast(a)),
        AST::Conj(a) => format!("conj({})", show_ast(a)),
        AST::Re(a) => format!("re({})", show_ast(a)),
        AST::Im(a) => format!("im({})", show_ast(a)),
    }
}

fn show_paren(ast: &AST) -> String {
    match ast {
        AST::Add(_, _) | AST::Sub(_, _) => format!("({})", show_ast(ast)),
        _ => show_ast(ast),
    }
}

fn show_factor(ast: &AST) -> String {
    match ast {
        AST::Add(_, _) | AST::Sub(_, _) => format!("({})", show_ast(ast)),
        _ => show_ast(ast),
    }
}
