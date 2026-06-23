use crate::ast::AST;
use crate::complex::C;

pub fn eval(ast: &AST, x: C, ans: C) -> C {
    match ast {
        AST::Num(n) => C::from(*n),
        AST::Pi => C::from(std::f64::consts::PI),
        AST::E => C::from(std::f64::consts::E),
        AST::Tau => C::from(std::f64::consts::TAU),
        AST::Phi => C::from((1.0 + 5.0_f64.sqrt()) / 2.0),
        AST::I => C::new(0.0, 1.0),
        AST::Var => x,
        AST::Ans => ans,
        AST::Add(a, b) => eval(a, x, ans) + eval(b, x, ans),
        AST::Sub(a, b) => eval(a, x, ans) - eval(b, x, ans),
        AST::Mul(a, b) => eval(a, x, ans) * eval(b, x, ans),
        AST::Div(a, b) => eval(a, x, ans) / eval(b, x, ans),
        AST::Pow(a, b) => eval(a, x, ans).pow(eval(b, x, ans)),
        AST::Gt(a, b) => {
            let va = eval(a, x, ans);
            let vb = eval(b, x, ans);
            if va.re > vb.re { C::from(1.0) } else { C::from(0.0) }
        }
        AST::Ge(a, b) => {
            let va = eval(a, x, ans);
            let vb = eval(b, x, ans);
            if va.re >= vb.re { C::from(1.0) } else { C::from(0.0) }
        }
        AST::Lt(a, b) => {
            let va = eval(a, x, ans);
            let vb = eval(b, x, ans);
            if va.re < vb.re { C::from(1.0) } else { C::from(0.0) }
        }
        AST::Le(a, b) => {
            let va = eval(a, x, ans);
            let vb = eval(b, x, ans);
            if va.re <= vb.re { C::from(1.0) } else { C::from(0.0) }
        }
        AST::Eqq(a, b) => {
            let va = eval(a, x, ans);
            let vb = eval(b, x, ans);
            if (va.re - vb.re).abs() < 1e-12 { C::from(1.0) } else { C::from(0.0) }
        }
        AST::Neq(a, b) => {
            let va = eval(a, x, ans);
            let vb = eval(b, x, ans);
            if (va.re - vb.re).abs() >= 1e-12 { C::from(1.0) } else { C::from(0.0) }
        }
        AST::Sin(a) => eval(a, x, ans).sin(),
        AST::Cos(a) => eval(a, x, ans).cos(),
        AST::Tan(a) => eval(a, x, ans).tan(),
        AST::Csc(a) => eval(a, x, ans).csc(),
        AST::Sec(a) => eval(a, x, ans).sec(),
        AST::Cot(a) => eval(a, x, ans).cot(),
        AST::Asin(a) => eval(a, x, ans).asin(),
        AST::Acos(a) => eval(a, x, ans).acos(),
        AST::Atan(a) => eval(a, x, ans).atan(),
        AST::Acsc(a) => eval(a, x, ans).acsc(),
        AST::Asec(a) => eval(a, x, ans).asec(),
        AST::Acot(a) => eval(a, x, ans).acot(),
        AST::Sinh(a) => eval(a, x, ans).sinh(),
        AST::Cosh(a) => eval(a, x, ans).cosh(),
        AST::Tanh(a) => eval(a, x, ans).tanh(),
        AST::Csch(a) => eval(a, x, ans).csch(),
        AST::Sech(a) => eval(a, x, ans).sech(),
        AST::Coth(a) => eval(a, x, ans).coth(),
        AST::Asinh(a) => eval(a, x, ans).asinh(),
        AST::Acosh(a) => eval(a, x, ans).acosh(),
        AST::Atanh(a) => eval(a, x, ans).atanh(),
        AST::Log(a) => eval(a, x, ans).ln(),
        AST::Log2(a) => eval(a, x, ans).log2(),
        AST::Log10(a) => eval(a, x, ans).log10(),
        AST::Log1p(a) => eval(a, x, ans).log1p(),
        AST::Exp(a) => eval(a, x, ans).exp(),
        AST::Expm1(a) => eval(a, x, ans).expm1(),
        AST::Sqrt(a) => eval(a, x, ans).sqrt(),
        AST::Cbrt(a) => eval(a, x, ans).cbrt(),
        AST::Abs(a) => C::from(eval(a, x, ans).abs()),
        AST::Sign(a) => {
            let v = eval(a, x, ans);
            if v.re > 0.0 { C::from(1.0) } else if v.re < 0.0 { C::from(-1.0) } else { C::from(0.0) }
        }
        AST::Floor(a) => C::from(eval(a, x, ans).re.floor()),
        AST::Ceil(a) => C::from(eval(a, x, ans).re.ceil()),
        AST::Round(a) => C::from(eval(a, x, ans).re.round()),
        AST::Gamma(a) => eval(a, x, ans).gamma(),
        AST::Erf(a) => eval(a, x, ans).erf(),
        AST::Conj(a) => eval(a, x, ans).conj(),
        AST::Re(a) => C::from(eval(a, x, ans).re),
        AST::Im(a) => C::from(eval(a, x, ans).im),
    }
}

pub fn eval_real(ast: &AST, x: f64) -> f64 {
    eval(ast, C::from(x), C::from(0.0)).re
}

pub fn eval_c(ast: &AST, x: f64) -> C {
    eval(ast, C::from(x), C::from(0.0))
}
