use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct C {
    pub re: f64,
    pub im: f64,
}

impl C {
    pub fn new(re: f64, im: f64) -> Self {
        C { re, im }
    }

    pub fn from(re: f64) -> Self {
        C { re, im: 0.0 }
    }

    pub fn abs(self) -> f64 {
        (self.re * self.re + self.im * self.im).sqrt()
    }

    pub fn arg(self) -> f64 {
        self.im.atan2(self.re)
    }

    pub fn conj(self) -> Self {
        C::new(self.re, -self.im)
    }

    pub fn ln(self) -> Self {
        C::new(self.abs().ln(), self.arg())
    }

    pub fn pow(self, b: C) -> Self {
        if self.re == 0.0 && self.im == 0.0 {
            return C::from(0.0);
        }
        let r = self.abs();
        let theta = self.arg();
        let lnr = r.ln();
        let new_r = (lnr * b.re - theta * b.im).exp();
        let new_theta = theta * b.re + lnr * b.im;
        C::new(new_r * new_theta.cos(), new_r * new_theta.sin())
    }

    pub fn sin(self) -> Self {
        C::new(self.re.sin() * self.im.cosh(), self.re.cos() * self.im.sinh())
    }

    pub fn cos(self) -> Self {
        C::new(self.re.cos() * self.im.cosh(), -self.re.sin() * self.im.sinh())
    }

    pub fn tan(self) -> Self {
        self.sin() / self.cos()
    }

    pub fn csc(self) -> Self {
        C::from(1.0) / self.sin()
    }

    pub fn sec(self) -> Self {
        C::from(1.0) / self.cos()
    }

    pub fn cot(self) -> Self {
        C::from(1.0) / self.tan()
    }

    pub fn asin(self) -> Self {
        C::new(0.0, -1.0) * ((C::from(1.0) - self * self).pow(C::from(0.5)) + C::new(0.0, 1.0) * self).ln()
    }

    pub fn acos(self) -> Self {
        C::from(std::f64::consts::FRAC_PI_2) - self.asin()
    }

    pub fn atan(self) -> Self {
        C::new(0.0, 0.5) * ((C::from(1.0) - C::new(0.0, 1.0) * self) / (C::from(1.0) + C::new(0.0, 1.0) * self)).ln()
    }

    pub fn sinh(self) -> Self {
        C::new(self.re.sinh() * self.im.cos(), self.re.cosh() * self.im.sin())
    }

    pub fn cosh(self) -> Self {
        C::new(self.re.cosh() * self.im.cos(), self.re.sinh() * self.im.sin())
    }

    pub fn tanh(self) -> Self {
        self.sinh() / self.cosh()
    }

    pub fn csch(self) -> Self {
        C::from(1.0) / self.sinh()
    }

    pub fn sech(self) -> Self {
        C::from(1.0) / self.cosh()
    }

    pub fn coth(self) -> Self {
        C::from(1.0) / self.tanh()
    }

    pub fn asinh(self) -> Self {
        (self + (self * self + C::from(1.0)).pow(C::from(0.5))).ln()
    }

    pub fn acosh(self) -> Self {
        (self + (self * self - C::from(1.0)).pow(C::from(0.5))).ln()
    }

    pub fn atanh(self) -> Self {
        C::new(0.0, 0.5) * ((C::from(1.0) + self) / (C::from(1.0) - self)).ln()
    }

    pub fn acsc(self) -> Self {
        C::from(1.0) / self.asin()
    }

    pub fn asec(self) -> Self {
        C::from(1.0) / self.acos()
    }

    pub fn acot(self) -> Self {
        C::from(1.0) / self.atan()
    }

    pub fn acsch(self) -> Self {
        C::from(1.0) / self.asinh()
    }

    pub fn asech(self) -> Self {
        C::from(1.0) / self.acosh()
    }

    pub fn acoth(self) -> Self {
        C::from(1.0) / self.atanh()
    }

    pub fn exp(self) -> Self {
        C::new(self.re.exp() * self.im.cos(), self.re.exp() * self.im.sin())
    }

    pub fn expm1(self) -> Self {
        self.exp() - C::from(1.0)
    }

    pub fn sqrt(self) -> Self {
        self.pow(C::from(0.5))
    }

    pub fn cbrt(self) -> Self {
        self.pow(C::from(1.0 / 3.0))
    }

    pub fn is_finite(self) -> bool {
        self.re.is_finite() || self.im.is_finite()
    }

    pub fn gamma(self) -> Self {
        if self.re < 0.5 {
            C::from(std::f64::consts::PI) / (C::from(std::f64::consts::PI) * self).sin() * Self::gamma(C::from(1.0) - self)
        } else {
            let g = 7.0;
            let t = self + C::from(g) - C::from(0.5);
            let p = [
                0.99999999999980993, 676.5203681218851, -1259.1392167224028,
                771.32342877765313, -176.61502916214059, 12.507343278686905,
                -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7,
            ];
            let p0 = C::from(p[0]);
            let mut sum = C::from(0.0);
            for (i, pi) in p.iter().skip(1).enumerate() {
                sum = sum + C::from(*pi) / (self + C::from((i + 1) as f64));
            }
            let series = p0 + sum;
            C::from((std::f64::consts::TAU.sqrt() / 2.0).sqrt()) * t.pow(self - C::from(0.5)) * (-t).exp() * series
        }
    }

    pub fn erf(self) -> Self {
        if self.re >= 0.0 {
            let t = C::from(1.0) / (C::from(1.0) + C::from(0.3275911) * self);
            let a = [0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429];
            let mut poly = C::from(0.0);
            for &ai in a.iter() {
                poly = (poly + C::from(ai)) * t;
            }
            C::from(1.0) - t * (-(self * self) - C::from(1.26551223)).exp() * poly
        } else {
            -Self::erf(-self)
        }
    }

    pub fn log2(self) -> Self {
        self.ln() / C::from(std::f64::consts::LN_2)
    }

    pub fn log10(self) -> Self {
        self.ln() / C::from(std::f64::consts::LN_10)
    }

    pub fn log1p(self) -> Self {
        (C::from(1.0) + self).ln()
    }
}

impl Add for C {
    type Output = C;
    fn add(self, b: C) -> C {
        C::new(self.re + b.re, self.im + b.im)
    }
}

impl Sub for C {
    type Output = C;
    fn sub(self, b: C) -> C {
        C::new(self.re - b.re, self.im - b.im)
    }
}

impl Mul for C {
    type Output = C;
    fn mul(self, b: C) -> C {
        C::new(self.re * b.re - self.im * b.im, self.re * b.im + self.im * b.re)
    }
}

impl Div for C {
    type Output = C;
    fn div(self, b: C) -> C {
        let d = b.re * b.re + b.im * b.im;
        C::new((self.re * b.re + self.im * b.im) / d, (self.im * b.re - self.re * b.im) / d)
    }
}

impl Neg for C {
    type Output = C;
    fn neg(self) -> C {
        C::new(-self.re, -self.im)
    }
}

impl fmt::Display for C {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.im.abs() < 1e-12 {
            return write!(f, "{}", nice_num(self.re));
        }
        if self.re.abs() < 1e-12 {
            if (self.im - 1.0).abs() < 1e-12 {
                return write!(f, "i");
            }
            if (self.im + 1.0).abs() < 1e-12 {
                return write!(f, "-i");
            }
            return write!(f, "{}i", nice_num(self.im));
        }
        let s = if self.im < 0.0 { " - " } else { " + " };
        write!(f, "{}{}{}", nice_num(self.re), s, nice_num(self.im.abs()))
    }
}

pub fn nice_num(n: f64) -> String {
    if n.is_nan() {
        return "undefined".into();
    }
    if !n.is_finite() {
        return if n > 0.0 { "∞".into() } else { "-∞".into() };
    }
    if n.abs() < 1e-12 {
        return "0".into();
    }
    if n.fract() == 0.0 && n.abs() < 1e15 {
        return format!("{}", n as i64);
    }
    let s = format!("{:.10}", n);
    let s = s.trim_end_matches('0');
    let s = s.trim_end_matches('.');
    if s.is_empty() { "0".into() } else { s.to_string() }
}
