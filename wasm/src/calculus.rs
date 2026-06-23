type Func = dyn Fn(f64) -> f64;

fn simpson(f: &Func, a: f64, b: f64) -> f64 {
    let m = (a + b) / 2.0;
    let h = (b - a) / 2.0;
    h / 3.0 * (f(a) + 4.0 * f(m) + f(b))
}

fn adapt_simpson_go(f: &Func, a: f64, b: f64, whole: f64, tol: f64) -> f64 {
    let m = (a + b) / 2.0;
    let left = simpson(f, a, m);
    let right = simpson(f, m, b);
    if (left + right - whole).abs() < 15.0 * tol {
        left + right + (left + right - whole) / 15.0
    } else {
        adapt_simpson_go(f, a, m, left, tol / 2.0) + adapt_simpson_go(f, m, b, right, tol / 2.0)
    }
}

pub fn adapt_simpson(f: &Func, a: f64, b: f64) -> f64 {
    let a = if a == f64::INFINITY { 1e6 } else if a == f64::NEG_INFINITY { -1e6 } else { a };
    let b = if b == f64::INFINITY { 1e6 } else if b == f64::NEG_INFINITY { -1e6 } else { b };
    adapt_simpson_go(f, a, b, simpson(f, a, b), 1e-8)
}

pub fn derivative(f: &Func, x: f64) -> f64 {
    let h = 1e-8;
    (f(x + h) - f(x - h)) / (2.0 * h)
}

pub fn derivative2(f: &Func, x: f64) -> f64 {
    let h = 1e-5;
    (f(x - h) - 2.0 * f(x) + f(x + h)) / (h * h)
}

pub fn derivative_n(n: u32, f: &Func, x: f64) -> f64 {
    match n {
        0 => f(x),
        1 => derivative(f, x),
        2 => derivative2(f, x),
        _ => {
            let h = (1e-3 / n as f64).max(1e-8);
            (derivative_n(n - 1, f, x + h) - derivative_n(n - 1, f, x - h)) / (2.0 * h)
        }
    }
}

pub fn limit(f: &Func, a: f64) -> f64 {
    f(a + 1e-10)
}

pub fn taylor_series(f: &Func, a: f64, n: u32, at_x: f64) -> f64 {
    let mut sum = 0.0;
    let mut fact = 1.0;
    for i in 0..=n {
        let term = derivative_n(i, f, a) / fact * (at_x - a).powi(i as i32);
        sum += term;
        fact *= (i + 1) as f64;
    }
    sum
}
