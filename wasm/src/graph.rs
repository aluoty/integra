use crate::ast::AST;
use crate::evaluator::eval_real;

fn nice_num(n: f64) -> String {
    if !n.is_finite() { return if n > 0.0 { "∞".into() } else { "-∞".into() }; }
    if n.abs() < 1e-12 { return "0".into(); }
    if n.fract() == 0.0 && n.abs() < 1e15 { return format!("{}", n as i64); }
    let s = format!("{:.10}", n);
    let s = s.trim_end_matches('0');
    let s = s.trim_end_matches('.');
    if s.is_empty() { "0".into() } else { s.to_string() }
}

pub fn generate_graph_svg(
    ast: &AST, x_min: f64, x_max: f64,
    y_min_opt: Option<f64>, y_max_opt: Option<f64>,
) -> String {
    let w = 600.0;
    let h = 400.0;
    let steps = 400;
    let mut pts: Vec<(f64, f64)> = Vec::new();
    for i in 0..=steps {
        let x = x_min + (x_max - x_min) * i as f64 / steps as f64;
        let y = eval_real(ast, x);
        pts.push((x, y));
    }
    let finite_pts: Vec<(f64, f64)> = pts.iter().filter(|(_, y)| y.is_finite()).copied().collect();
    if finite_pts.is_empty() {
        return format!(
            "<svg xmlns='http://www.w3.org/2000/svg' width='{w:.0}' height='{h:.0}'><text x='10' y='20' fill='#666'>No points to display</text></svg>",
            w = w, h = h
        );
    }
    let ys: Vec<f64> = finite_pts.iter().map(|p| p.1).collect();
    let (min_y, max_y) = ys.iter().cloned().fold(
        (f64::INFINITY, f64::NEG_INFINITY),
        |(mn, mx), y| (mn.min(y), mx.max(y)),
    );
    let y_lo = y_min_opt.unwrap_or(min_y - (max_y - min_y) * 0.1);
    let y_hi = y_max_opt.unwrap_or(max_y + (max_y - min_y) * 0.1);
    let y_lo = if y_lo == y_hi { y_lo - 1.0 } else { y_lo };
    let y_hi = if y_lo == y_hi { y_hi + 1.0 } else { y_hi };
    let mx = |x: f64| (x - x_min) / (x_max - x_min) * w;
    let my = |y: f64| h - (y - y_lo) / (y_hi - y_lo) * h;
    let mut path = String::new();
    let mut prev_finite = true;
    for (i, (x, y)) in pts.iter().enumerate() {
        if !y.is_finite() { prev_finite = false; continue; }
        if i == 0 || !prev_finite {
            path.push_str(&format!(" M {:.2},{:.2}", mx(*x), my(*y)));
        } else {
            path.push_str(&format!(" L {:.2},{:.2}", mx(*x), my(*y)));
        }
        prev_finite = true;
    }
    let mut grid_x = String::new();
    let mut grid_y = String::new();
    let mut labels_x = String::new();
    let mut labels_y = String::new();
    for i in 0..=10 {
        let t = i as f64 / 10.0;
        let xv = x_min + (x_max - x_min) * t;
        let yv = y_lo + (y_hi - y_lo) * t;
        let px = mx(xv);
        let py = my(yv);
        grid_x.push_str(&format!(
            "<line x1='{:.2}' y1='0' x2='{:.2}' y2='{:.2}' stroke='#ddd' stroke-width='0.5'/>",
            px, px, h
        ));
        labels_x.push_str(&format!(
            "<text x='{:.2}' y='{:.2}' text-anchor='middle' font-size='10' fill='#666'>{}</text>",
            px, h - 5.0, nice_num(xv)
        ));
        grid_y.push_str(&format!(
            "<line x1='0' y1='{:.2}' x2='{:.2}' y2='{:.2}' stroke='#ddd' stroke-width='0.5'/>",
            py, w, py
        ));
        labels_y.push_str(&format!(
            "<text x='5' y='{:.2}' text-anchor='start' font-size='10' fill='#666'>{}</text>",
            py - 3.0, nice_num(yv)
        ));
    }
    let x0 = mx(0.0);
    let y0 = my(0.0);
    format!(
        "<svg xmlns='http://www.w3.org/2000/svg' width='{w:.0}' height='{h:.0}'>\
<rect width='{w:.0}' height='{h:.0}' fill='white'/>\
{gx}{gy}{lx}{ly}\
<line x1='{x0:.2}' y1='0' x2='{x0:.2}' y2='{h:.0}' stroke='#999' stroke-width='1'/>\
<line x1='0' y1='{y0:.2}' x2='{w:.0}' y2='{y0:.2}' stroke='#999' stroke-width='1'/>\
<path d='{path}' fill='none' stroke='#2563eb' stroke-width='2'/>\
</svg>",
        w = w, h = h, gx = grid_x, gy = grid_y, lx = labels_x, ly = labels_y,
        x0 = x0, y0 = y0, path = path,
    )
}

pub fn generate_integral_svg(ast: &AST, a: f64, b: f64) -> String {
    let expr = ast;
    let pad = (b - a) * 0.1;
    let x_min = a - pad;
    let x_max = b + pad;
    let w = 600.0;
    let h = 400.0;
    let steps = 400;
    let mut pts: Vec<(f64, f64)> = Vec::new();
    for i in 0..=steps {
        let x = x_min + (x_max - x_min) * i as f64 / steps as f64;
        let y = eval_real(expr, x);
        pts.push((x, y));
    }
    let finite_pts: Vec<(f64, f64)> = pts.iter().filter(|(_, y)| y.is_finite()).copied().collect();
    if finite_pts.is_empty() {
        return format!(
            "<svg xmlns='http://www.w3.org/2000/svg' width='{w:.0}' height='{h:.0}'><text x='10' y='20' fill='#666'>No points</text></svg>",
            w = w, h = h
        );
    }
    let ys: Vec<f64> = finite_pts.iter().map(|p| p.1).collect();
    let (min_y, max_y) = ys.iter().cloned().fold(
        (f64::INFINITY, f64::NEG_INFINITY),
        |(mn, mx), y| (mn.min(y), mx.max(y)),
    );
    let y_lo = min_y - (max_y - min_y) * 0.1;
    let y_hi = max_y + (max_y - min_y) * 0.1;
    let y_lo = if y_lo == y_hi { y_lo - 1.0 } else { y_lo };
    let y_hi = if y_lo == y_hi { y_hi + 1.0 } else { y_hi };
    let mx = |x: f64| (x - x_min) / (x_max - x_min) * w;
    let my = |y: f64| h - (y - y_lo) / (y_hi - y_lo) * h;
    let mut path = String::new();
    let mut prev_finite = true;
    for (i, (x, y)) in pts.iter().enumerate() {
        if !y.is_finite() { prev_finite = false; continue; }
        if i == 0 || !prev_finite {
            path.push_str(&format!(" M {:.2},{:.2}", mx(*x), my(*y)));
        } else {
            path.push_str(&format!(" L {:.2},{:.2}", mx(*x), my(*y)));
        }
        prev_finite = true;
    }
    let shade_pts: Vec<(f64, f64)> = finite_pts
        .iter()
        .filter(|(x, _)| *x >= a && *x <= b)
        .copied()
        .collect();
    let mut shade = String::new();
    if !shade_pts.is_empty() {
        let bottom = my(0.0);
        shade.push_str(&format!(" M {:.2},{:.2}", mx(shade_pts[0].0), bottom));
        shade.push_str(&format!(" L {:.2},{:.2}", mx(shade_pts[0].0), my(shade_pts[0].1)));
        for (x, y) in shade_pts.iter().skip(1) {
            shade.push_str(&format!(" L {:.2},{:.2}", mx(*x), my(*y)));
        }
        shade.push_str(&format!(
            " L {:.2},{:.2} Z",
            mx(shade_pts.last().unwrap().0),
            bottom
        ));
    }
    let mut grid_x = String::new();
    let mut grid_y = String::new();
    let mut labels_x = String::new();
    let mut labels_y = String::new();
    for i in 0..=10 {
        let t = i as f64 / 10.0;
        let xv = x_min + (x_max - x_min) * t;
        let yv = y_lo + (y_hi - y_lo) * t;
        let px = mx(xv);
        let py = my(yv);
        grid_x.push_str(&format!(
            "<line x1='{:.2}' y1='0' x2='{:.2}' y2='{:.2}' stroke='#ddd' stroke-width='0.5'/>",
            px, px, h
        ));
        labels_x.push_str(&format!(
            "<text x='{:.2}' y='{:.2}' text-anchor='middle' font-size='10' fill='#666'>{}</text>",
            px, h - 5.0, nice_num(xv)
        ));
        grid_y.push_str(&format!(
            "<line x1='0' y1='{:.2}' x2='{:.2}' y2='{:.2}' stroke='#ddd' stroke-width='0.5'/>",
            py, w, py
        ));
        labels_y.push_str(&format!(
            "<text x='5' y='{:.2}' text-anchor='start' font-size='10' fill='#666'>{}</text>",
            py - 3.0, nice_num(yv)
        ));
    }
    let x0 = mx(0.0);
    let y0 = my(0.0);
    format!(
        "<svg xmlns='http://www.w3.org/2000/svg' width='{w:.0}' height='{h:.0}'>\
<rect width='{w:.0}' height='{h:.0}' fill='white'/>\
{gx}{gy}{lx}{ly}\
<line x1='{x0:.2}' y1='0' x2='{x0:.2}' y2='{h:.0}' stroke='#999' stroke-width='1'/>\
<line x1='0' y1='{y0:.2}' x2='{w:.0}' y2='{y0:.2}' stroke='#999' stroke-width='1'/>\
{shade}\
<path d='{path}' fill='none' stroke='#2563eb' stroke-width='2'/>\
</svg>",
        w = w, h = h, gx = grid_x, gy = grid_y, lx = labels_x, ly = labels_y,
        x0 = x0, y0 = y0, shade = shade, path = path,
    )
}
