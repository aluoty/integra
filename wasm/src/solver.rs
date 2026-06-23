use crate::ast::AST;
use crate::complex::{C, nice_num};
use crate::evaluator::eval_real;

fn has_var(ast: &AST) -> bool {
    match ast {
        AST::Var => true,
        AST::Num(_) => false,
        AST::Add(a, b) | AST::Sub(a, b) | AST::Mul(a, b) | AST::Div(a, b) | AST::Pow(a, b) => {
            has_var(a) || has_var(b)
        }
        AST::Sin(a) | AST::Cos(a) | AST::Tan(a) | AST::Exp(a) | AST::Log(a) | AST::Sqrt(a)
        | AST::Abs(a) => has_var(a),
        _ => false,
    }
}

fn solve_from_expr(ast: &AST) -> AST {
    match ast {
        AST::Eqq(l, r) => AST::Sub(l.clone(), r.clone()),
        _ => ast.clone(),
    }
}

pub fn solve_linear(ast: &AST) -> String {
    let expr = solve_from_expr(ast);
    if !has_var(&expr) { return "No variable x found in expression".into(); }
    let b = eval_real(&expr, 0.0);
    let a = eval_real(&expr, 1.0) - b;
    if a.abs() < 1e-12 {
        if b.abs() < 1e-12 { return "Infinite solutions (identity)".into(); }
        return "No solution".into();
    }
    format!("x = {}", nice_num(-b / a))
}

pub fn solve_quadratic(ast: &AST) -> String {
    let expr = solve_from_expr(ast);
    if !has_var(&expr) { return "No variable x found in expression".into(); }
    let c = eval_real(&expr, 0.0);
    let b = (eval_real(&expr, 1.0) - eval_real(&expr, -1.0)) / 2.0;
    let a = eval_real(&expr, 1.0) - b - c;
    if a.abs() < 1e-12 { return solve_linear(&expr); }
    let disc = b * b - 4.0 * a * c;
    if disc < 0.0 {
        let real = -b / (2.0 * a);
        let imag = (-disc).sqrt() / (2.0 * a);
        return format!("x = {} ± {}i", nice_num(real), nice_num(imag));
    }
    let sqrt_d = disc.sqrt();
    let x1 = (-b + sqrt_d) / (2.0 * a);
    let x2 = (-b - sqrt_d) / (2.0 * a);
    if disc.abs() < 1e-12 {
        format!("x = {} (repeated)", nice_num(x1))
    } else {
        format!("x₁ = {}\nx₂ = {}", nice_num(x1), nice_num(x2))
    }
}

pub fn solve_cubic(ast: &AST) -> String {
    let expr = solve_from_expr(ast);
    if !has_var(&expr) { return "No variable x found in expression".into(); }
    let d = eval_real(&expr, 0.0);
    let f1 = eval_real(&expr, 1.0);
    let fm1 = eval_real(&expr, -1.0);
    let f2 = eval_real(&expr, 2.0);
    let b = (f1 + fm1) / 2.0 - d;
    let ac = (f1 - fm1) / 2.0;
    let a = (f2 - 4.0 * b - 2.0 * ac - d) / 6.0;
    let c = ac - a;
    if a.abs() < 1e-12 { return solve_quadratic(&expr); }
    let p = (3.0 * a * c - b * b) / (3.0 * a * a);
    let q = (2.0 * b * b * b - 9.0 * a * b * c + 27.0 * a * a * d) / (27.0 * a * a * a);
    let disc = q * q / 4.0 + p * p * p / 27.0;
    let omega = C::new(-0.5, 3.0_f64.sqrt() / 2.0);
    let omega2 = omega * omega;
    let offset = b / (3.0 * a);
    let u = C::from(-q / 2.0) + C::from(disc.sqrt());
    let v = C::from(-q / 2.0) - C::from(disc.sqrt());
    let cu = u.pow(C::from(1.0 / 3.0));
    let cv = v.pow(C::from(1.0 / 3.0));
    let x1 = cu + cv - C::from(offset);
    let x2 = cu * omega + cv * omega2 - C::from(offset);
    let x3 = cu * omega2 + cv * omega - C::from(offset);
    let eps = 1e-10;
    if (x1 - x2).abs() < eps && (x2 - x3).abs() < eps {
        format!("x = {} (triple)", x1)
    } else if (x1 - x2).abs() < eps || (x2 - x3).abs() < eps {
        let repeated = x1;
        let single = if (x1 - x2).abs() < eps { x3 } else { x1 };
        format!("x₁ = {} (repeated)\nx₂ = {}", repeated, single)
    } else {
        format!("x₁ = {}\nx₂ = {}\nx₃ = {}", x1, x2, x3)
    }
}

pub fn maybe_show_roots(ast: &AST) -> Option<String> {
    match ast {
        AST::Pow(a, b) => {
            match (a.as_ref(), b.as_ref()) {
                (AST::Num(a_val), AST::Div(_, _)) | (AST::Num(a_val), AST::Pow(_, _)) => {
                    let n = match b.as_ref() {
                        AST::Div(num, _) => {
                            if let AST::Num(n) = num.as_ref() { *n } else { return None; }
                        }
                        _ => return None,
                    };
                    if n > 1.0 && (n.round() - n).abs() < 1e-12 {
                        let ni = n.round() as i32;
                        let r = a_val.abs().powf(1.0 / n);
                        let theta = if *a_val >= 0.0 { 0.0 } else { std::f64::consts::PI };
                        let roots: Vec<String> = (0..ni)
                            .map(|k| {
                                let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n;
                                C::new(r * angle.cos(), r * angle.sin()).to_string()
                            })
                            .collect();
                        return Some(format!("All {} roots:\n  {}", ni, roots.join("\n  ")));
                    }
                    None
                }
                _ => None,
            }
        }
        _ => None,
    }
}
