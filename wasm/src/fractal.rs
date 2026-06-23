use crate::complex::C;

fn mandelbrot_iter(c: C, max_iter: u32) -> f64 {
    let mut z = C::new(0.0, 0.0);
    for n in 0..max_iter {
        if z.abs() > 2.0 {
            return n as f64 + 1.0 - (z.abs().ln().ln()) / 2.0f64.ln();
        }
        z = z * z + c;
    }
    0.0
}

fn julia_iter(z0: C, c: C, max_iter: u32) -> f64 {
    let mut z = z0;
    for n in 0..max_iter {
        if z.abs() > 2.0 {
            return n as f64 + 1.0 - (z.abs().ln().ln()) / 2.0f64.ln();
        }
        z = z * z + c;
    }
    0.0
}

fn burning_ship_iter(c: C, max_iter: u32) -> f64 {
    let mut z = C::new(0.0, 0.0);
    for n in 0..max_iter {
        if z.abs() > 2.0 {
            return n as f64 + 1.0 - (z.abs().ln().ln()) / 2.0f64.ln();
        }
        z = C::new(
            z.re * z.re - z.im * z.im + c.re,
            2.0 * (z.re * z.im).abs() + c.im,
        );
    }
    0.0
}

fn smooth_color(mu: f64, max_iter: u32) -> String {
    if mu == 0.0 { return "#000".into(); }
    let t = mu / max_iter as f64;
    let hue = 360.0 * 4.0 * t;
    let light = 45.0 + 40.0 * (std::f64::consts::PI * t).sin();
    format!("hsl({},100%,{}%)", hue, light)
}

pub fn generate_fractal_svg(
    kind: &str, w: u32, h: u32, max_iter: u32,
    x_min: f64, x_max: f64, y_min: f64, y_max: f64,
    cx_opt: Option<f64>, cy_opt: Option<f64>,
) -> String {
    let x_step = (x_max - x_min) / w as f64;
    let y_step = (y_max - y_min) / h as f64;
    let c = if kind == "julia" {
        C::new(cx_opt.unwrap_or(-0.7), cy_opt.unwrap_or(0.27015))
    } else {
        C::new(0.0, 0.0)
    };
    let mut rects = String::new();
    for py in 0..h {
        for px in 0..w {
            let x0 = x_min + px as f64 * x_step;
            let y0 = y_min + py as f64 * y_step;
            let mu = match kind {
                "julia" => julia_iter(C::new(x0, y0), c, max_iter),
                "burningship" => burning_ship_iter(C::new(x0, y0), max_iter),
                _ => mandelbrot_iter(C::new(x0, y0), max_iter),
            };
            rects.push_str(&format!(
                r#"<rect x="{}" y="{}" width="1" height="1" fill="{}"/>"#,
                px, py, smooth_color(mu, max_iter),
            ));
        }
    }
    format!(
        r#"<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}">{rects}</svg>"#,
        w = w, h = h, rects = rects,
    )
}
