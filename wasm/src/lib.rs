mod antideriv;
mod ast;
mod calculus;
mod complex;
mod derive;
mod evaluator;
mod fractal;
mod graph;
mod parser;
mod solver;
mod token;

use wasm_bindgen::prelude::*;
use crate::ast::AST;
use crate::parser::parse;
use crate::evaluator::eval;
use crate::derive::{deriv, show_ast};
use crate::calculus::{adapt_simpson, derivative, derivative2, derivative_n, limit, taylor_series};
use crate::graph::{generate_graph_svg, generate_integral_svg};
use crate::fractal::generate_fractal_svg;

fn parse_or_err(s: &str) -> Result<AST, String> {
    parse(s).ok_or_else(|| "cannot parse".into())
}

fn cmd_error(msg: &str) -> String {
    format!("Error: {}", msg)
}

fn nice_num(n: f64) -> String {
    complex::nice_num(n)
}

fn show_complex(re: f64, im: f64) -> String {
    if im.abs() < 1e-12 { return nice_num(re); }
    if re.abs() < 1e-12 {
        if (im - 1.0).abs() < 1e-12 { return "i".into(); }
        if (im + 1.0).abs() < 1e-12 { return "-i".into(); }
        return format!("{}i", nice_num(im));
    }
    let s = if im < 0.0 { " - " } else { " + " };
    format!("{}{}{}", nice_num(re), s, nice_num(im.abs()))
}

#[wasm_bindgen]
pub fn evaluate(expr: &str) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let r = eval(&ast, complex::C::from(0.0), complex::C::from(0.0));
    if r.is_finite() {
        r.to_string()
    } else {
        cmd_error("invalid expression")
    }
}

#[wasm_bindgen]
pub fn solve_linear(expr: &str) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    crate::solver::solve_linear(&ast)
}

#[wasm_bindgen]
pub fn solve_quadratic(expr: &str) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    crate::solver::solve_quadratic(&ast)
}

#[wasm_bindgen]
pub fn solve_cubic(expr: &str) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    crate::solver::solve_cubic(&ast)
}

#[wasm_bindgen]
pub fn symbolic_deriv(expr: &str) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    show_ast(&deriv(&ast))
}

#[wasm_bindgen]
pub fn derivative_at(expr: &str, x: f64) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
    format!("{}", derivative(&f, x))
}

#[wasm_bindgen]
pub fn derivative2_at(expr: &str, x: f64) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
    format!("{}", derivative2(&f, x))
}

#[wasm_bindgen]
pub fn derivative_n_at(expr: &str, n: u32, x: f64) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
    format!("{}", derivative_n(n, &f, x))
}

#[wasm_bindgen]
pub fn integral(expr: &str, from: f64, to: f64) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
    format!("{}", nice_num(adapt_simpson(&f, from, to)))
}

#[wasm_bindgen]
pub fn limit_at(expr: &str, x: f64) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
    format!("{}", limit(&f, x))
}

#[wasm_bindgen]
pub fn taylor_at(expr: &str, a: f64, order: u32) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
    format!("{}", taylor_series(&f, a, order, 0.0))
}

#[wasm_bindgen]
pub fn graph_svg(
    expr: &str, x_min: f64, x_max: f64,
    y_min: f64, y_max: f64,
    auto_y: bool,
) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(_) => return String::new(),
    };
    let y_min_opt = if auto_y { None } else { Some(y_min) };
    let y_max_opt = if auto_y { None } else { Some(y_max) };
    generate_graph_svg(&ast, x_min, x_max, y_min_opt, y_max_opt)
}

#[wasm_bindgen]
pub fn integral_svg(expr: &str, a: f64, b: f64) -> String {
    match parse_or_err(expr) {
        Ok(ast) => generate_integral_svg(&ast, a, b),
        Err(_) => String::new(),
    }
}

#[wasm_bindgen]
pub fn mandelbrot_svg(
    w: u32, h: u32, max_iter: u32,
    x_min: f64, x_max: f64, y_min: f64, y_max: f64,
) -> String {
    generate_fractal_svg("mandelbrot", w, h, max_iter, x_min, x_max, y_min, y_max, None, None)
}

#[wasm_bindgen]
pub fn julia_svg(
    cx: f64, cy: f64, w: u32, h: u32, max_iter: u32,
    x_min: f64, x_max: f64, y_min: f64, y_max: f64,
) -> String {
    generate_fractal_svg("julia", w, h, max_iter, x_min, x_max, y_min, y_max, Some(cx), Some(cy))
}

#[wasm_bindgen]
pub fn burning_ship_svg(
    w: u32, h: u32, max_iter: u32,
    x_min: f64, x_max: f64, y_min: f64, y_max: f64,
) -> String {
    generate_fractal_svg("burningship", w, h, max_iter, x_min, x_max, y_min, y_max, None, None)
}

#[wasm_bindgen]
pub fn antideriv(expr: &str) -> String {
    let ast = match parse_or_err(expr) {
        Ok(a) => a,
        Err(e) => return cmd_error(&e),
    };
    match crate::antideriv::find_antiderivative_string(&ast) {
        Some(s) => format!("\u{222b} f(x) dx = {} + C", s),
        None => "Cannot find antiderivative".into(),
    }
}

#[wasm_bindgen]
pub fn process_command(cmd: &str) -> String {
    if !cmd.starts_with(':') {
        return evaluate(cmd);
    }
    let parts: Vec<&str> = cmd[1..].split_whitespace().collect();
    if parts.is_empty() { return String::new(); }
    let command = parts[0];
    let rest: String = parts[1..].join(" ");
    let rest = rest.trim();

    match command {
        "help" => {
            let mut s = String::from("Commands:\n");
            s.push_str("  :help                     Show this help\n");
            s.push_str("  :about                    About Integra\n");
            s.push_str("  :deriv <expr> [at <x>]    Derivative\n");
            s.push_str("  :deriv2 <expr> at <x>     Numerical 2nd derivative\n");
            s.push_str("  :derivn <expr> order <n> at <x>\n");
            s.push_str("  :integral <expr> from <a> to <b>\n");
            s.push_str("  :limit <expr> as <x>      Limit\n");
            s.push_str("  :taylor <expr> at <a> order <n>\n");
            s.push_str("  :solve <expr>             Solve linear\n");
            s.push_str("  :solveq <expr>            Solve quadratic\n");
            s.push_str("  :solvec <expr>            Solve cubic\n");
            s.push_str("  :graph <expr> [from <a> to <b>] [yfrom <ya> to <yb>]\n");
            s.push_str("  :mandelbrot [w h iter xmin xmax ymin ymax]\n");
            s.push_str("  :julia <cx> <cy> [w h iter xmin xmax ymin ymax]\n");
            s.push_str("  :burningship [w h iter xmin xmax ymin ymax]\n");
            s.push_str("  :explain deriv|integral|solve <expr>\n");
            s.push_str("  :clear                    Clear output\n");
            s
        }
        "about" => "Integra v1.0 — Web Calculator\nComplex numbers, calculus, graphing, fractals\nBuilt with Rust + WASM".into(),
        "deriv" => {
            let parts: Vec<&str> = rest.splitn(2, " at ").collect();
            let expr_str = parts[0];
            let at_str = parts.get(1).copied();
            let ast = match parse_or_err(expr_str) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            let d = deriv(&ast);
            let mut result = format!("f'(x) = {}", show_ast(&d));
            if let Some(at) = at_str {
                if let Some(at_ast) = parse(at) {
                    let val = eval(&at_ast, complex::C::from(0.0), complex::C::from(0.0));
                    let fprime = eval(&d, complex::C::from(val.re), complex::C::from(0.0));
                    result.push_str(&format!("\nf'({}) = {}", at, fprime));
                } else if let Ok(x) = at.parse::<f64>() {
                    let fprime = eval(&d, complex::C::from(x), complex::C::from(0.0));
                    result.push_str(&format!("\nf'({}) = {}", at, fprime));
                }
            }
            result
        }
        "deriv2" => {
            let parts: Vec<&str> = rest.splitn(2, " at ").collect();
            if parts.len() < 2 { return "Usage: :deriv2 <expr> at <x>".into(); }
            let ast = match parse_or_err(parts[0]) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            let x: f64 = match parts[1].parse() {
                Ok(n) => n,
                Err(_) => return "Error: invalid x".into(),
            };
            let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
            format!("f''({}) = {}", parts[1], derivative2(&f, x))
        }
        "derivn" => {
            let parts: Vec<&str> = rest.splitn(2, " order ").collect();
            if parts.len() < 2 { return "Usage: :derivn <expr> order <n> at <x>".into(); }
            let expr_str = parts[0];
            let rest2 = parts[1];
            let parts2: Vec<&str> = rest2.splitn(2, " at ").collect();
            if parts2.len() < 2 { return "Usage: :derivn <expr> order <n> at <x>".into(); }
            let ast = match parse_or_err(expr_str) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            let n: u32 = match parts2[0].parse() { Ok(v) => v, Err(_) => return "Error: invalid n".into() };
            let x: f64 = match parts2[1].parse() { Ok(v) => v, Err(_) => return "Error: invalid x".into() };
            let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
            format!("f^({})({}) = {}", parts2[0], parts2[1], derivative_n(n, &f, x))
        }
        "integral" => {
            let parts: Vec<&str> = rest.splitn(2, " from ").collect();
            if parts.len() < 2 {
                let ast = match parse_or_err(rest) {
                    Ok(a) => a,
                    Err(e) => return cmd_error(&e),
                };
                return match crate::antideriv::find_antiderivative_string(&ast) {
                    Some(s) => format!("\u{222b} f(x) dx = {} + C", s),
                    None => "Cannot find antiderivative".into(),
                };
            }
            let expr_str = parts[0];
            let rest2 = parts[1];
            let parts2: Vec<&str> = rest2.splitn(2, " to ").collect();
            if parts2.len() < 2 { return "Usage: :integral <expr> from <a> to <b>".into(); }
            let ast = match parse_or_err(expr_str) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            let a: f64 = match parts2[0].parse() { Ok(v) => v, Err(_) => return "Error: invalid a".into() };
            let b: f64 = match parts2[1].parse() { Ok(v) => v, Err(_) => return "Error: invalid b".into() };
            let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
            let r = adapt_simpson(&f, a, b);
            format!("\u{222b} f(x) dx from {} to {} = {}", parts2[0], parts2[1], nice_num(r))
        }
        "limit" => {
            let parts: Vec<&str> = rest.splitn(2, " as ").collect();
            if parts.len() < 2 { return "Usage: :limit <expr> as <x>".into(); }
            let ast = match parse_or_err(parts[0]) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            let x: f64 = match parts[1].parse() { Ok(v) => v, Err(_) => return "Error: invalid x".into() };
            let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
            format!("lim f(x) as x → {} = {}", parts[1], limit(&f, x))
        }
        "taylor" => {
            let parts: Vec<&str> = rest.splitn(2, " at ").collect();
            if parts.len() < 2 { return "Usage: :taylor <expr> at <a> order <n>".into(); }
            let expr_str = parts[0];
            let rest2 = parts[1];
            let parts2: Vec<&str> = rest2.splitn(2, " order ").collect();
            if parts2.len() < 2 { return "Usage: :taylor <expr> at <a> order <n>".into(); }
            let ast = match parse_or_err(expr_str) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            let a: f64 = match parts2[0].parse() { Ok(v) => v, Err(_) => return "Error: invalid a".into() };
            let n: u32 = match parts2[1].parse() { Ok(v) => v, Err(_) => return "Error: invalid n".into() };
            let n = n.min(10);
            let f = move |xv: f64| crate::evaluator::eval_real(&ast, xv);
            let r = taylor_series(&f, a, n, 0.0);
            format!("T_{}(0) ≈ {}", parts2[1], nice_num(r))
        }
        "solve" => {
            let rest_processed = process_eq(rest);
            solve_linear(&rest_processed)
        }
        "solveq" => {
            let rest_processed = process_eq(rest);
            solve_quadratic(&rest_processed)
        }
        "solvec" => {
            let rest_processed = process_eq(rest);
            solve_cubic(&rest_processed)
        }
        "graph" => {
            let parts: Vec<&str> = rest.splitn(2, " from ").collect();
            let expr_str = parts[0];
            let ast = match parse_or_err(expr_str) {
                Ok(a) => a,
                Err(_) => return String::new(),
            };
            let (x_min, x_max, y_min, y_max, auto_y) = if parts.len() < 2 {
                (-10.0, 10.0, 0.0, 0.0, true)
            } else {
                let rest2 = parts[1];
                let xy_parts: Vec<&str> = rest2.splitn(2, " yfrom ").collect();
                let range_parts: Vec<&str> = xy_parts[0].splitn(2, " to ").collect();
                if range_parts.len() < 2 { return String::new(); }
                let x_min: f64 = range_parts[0].parse().unwrap_or(-10.0);
                let x_max: f64 = range_parts[1].parse().unwrap_or(10.0);
                if xy_parts.len() < 2 {
                    (x_min, x_max, 0.0, 0.0, true)
                } else {
                    let y_parts: Vec<&str> = xy_parts[1].splitn(2, " to ").collect();
                    if y_parts.len() < 2 { (x_min, x_max, 0.0, 0.0, true) }
                    else {
                        let y_min: f64 = y_parts[0].parse().unwrap_or(0.0);
                        let y_max: f64 = y_parts[1].parse().unwrap_or(0.0);
                        (x_min, x_max, y_min, y_max, false)
                    }
                }
            };
            generate_graph_svg(
                &ast, x_min, x_max,
                if auto_y { None } else { Some(y_min) },
                if auto_y { None } else { Some(y_max) },
            )
        }
        "mandelbrot" | "mandel" => {
            let args: Vec<&str> = rest.split_whitespace().collect();
            let w: u32 = args.first().and_then(|s| s.parse().ok()).unwrap_or(100);
            let h: u32 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(100);
            let iter: u32 = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(50);
            let x_min: f64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(-2.5);
            let x_max: f64 = args.get(4).and_then(|s| s.parse().ok()).unwrap_or(1.0);
            let y_min: f64 = args.get(5).and_then(|s| s.parse().ok()).unwrap_or(-1.25);
            let y_max: f64 = args.get(6).and_then(|s| s.parse().ok()).unwrap_or(1.25);
            mandelbrot_svg(w, h, iter, x_min, x_max, y_min, y_max)
        }
        "julia" => {
            let args: Vec<&str> = rest.split_whitespace().collect();
            let cx: f64 = args.first().and_then(|s| s.parse().ok()).unwrap_or(-0.7);
            let cy: f64 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(0.27015);
            let w: u32 = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(100);
            let h: u32 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(100);
            let iter: u32 = args.get(4).and_then(|s| s.parse().ok()).unwrap_or(50);
            let x_min: f64 = args.get(5).and_then(|s| s.parse().ok()).unwrap_or(-2.0);
            let x_max: f64 = args.get(6).and_then(|s| s.parse().ok()).unwrap_or(2.0);
            let y_min: f64 = args.get(7).and_then(|s| s.parse().ok()).unwrap_or(-1.5);
            let y_max: f64 = args.get(8).and_then(|s| s.parse().ok()).unwrap_or(1.5);
            julia_svg(cx, cy, w, h, iter, x_min, x_max, y_min, y_max)
        }
        "burningship" | "ship" => {
            let args: Vec<&str> = rest.split_whitespace().collect();
            let w: u32 = args.first().and_then(|s| s.parse().ok()).unwrap_or(100);
            let h: u32 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(100);
            let iter: u32 = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(50);
            let x_min: f64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(-2.5);
            let x_max: f64 = args.get(4).and_then(|s| s.parse().ok()).unwrap_or(1.5);
            let y_min: f64 = args.get(5).and_then(|s| s.parse().ok()).unwrap_or(-2.0);
            let y_max: f64 = args.get(6).and_then(|s| s.parse().ok()).unwrap_or(1.0);
            burning_ship_svg(w, h, iter, x_min, x_max, y_min, y_max)
        }
        "explain" => {
            let parts: Vec<&str> = rest.splitn(2, " ").collect();
            if parts.len() < 2 { return "Usage: :explain deriv|integral|solve <expr>".into(); }
            let kind = parts[0];
            let expr_str = parts[1];
            let ast = match parse_or_err(expr_str) {
                Ok(a) => a,
                Err(e) => return cmd_error(&e),
            };
            match kind {
                "deriv" => format!("f(x) = {}\nf'(x) = {}", show_ast(&ast), show_ast(&deriv(&ast))),
                "integral" => {
                    match crate::antideriv::find_antiderivative_string(&ast) {
                        Some(s) => format!("f(x) = {}\n\u{222b} f(x) dx = {} + C", show_ast(&ast), s),
                        None => format!("f(x) = {}\n\u{222b} f(x) dx = ?", show_ast(&ast)),
                    }
                }
                "solve" => format!("Equation: {} = 0\nSolution: {}", show_ast(&ast), crate::solver::solve_linear(&ast)),
                _ => "Usage: :explain deriv|integral|solve <expr>".into(),
            }
        }
        "clear" => "CLEAR".into(),
        _ => format!("Unknown command. Type :help for available commands."),
    }
}

fn process_eq(s: &str) -> String {
    let mut parts = s.splitn(2, '=');
    let left = parts.next().unwrap_or("");
    if let Some(right) = parts.next() {
        format!("({})-({})", left, right)
    } else {
        s.to_string()
    }
}

#[wasm_bindgen]
pub fn init_panic_hook() {}
