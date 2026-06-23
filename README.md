# integra

A REPL calculator with complex numbers, symbolic differentiation, SVG graphing,
fractals, calculus, and algebra solving — compiled to WASM for the web.

## Quick start

```bash
./build.sh
```

Serve `dist/` or run `npm run dev` in `web/`.

> The `build.sh` script handles Rust installation, WASM target, `wasm-bindgen-cli`,
> and the NixOS linker workaround automatically. For Cloudflare Pages, set the
> build command to `./build.sh` and publish directory to `dist/`.

## Features

### Expressions

| Category               | Examples                                                              |
|------------------------|-----------------------------------------------------------------------|
| Arithmetic             | `2 + 3`, `4 * 5`, `10 / 2`, `2^3`                                    |
| Implicit multiplication| `4i`, `2x`, `sin(x)cos(x)`, `2(x+1)`                                 |
| Comparison             | `>`, `>=`, `<`, `<=`, `==`, `!=` (returns 0 or 1)                    |
| Trig                   | `sin(x)`, `cos(x)`, `tan(x)`                                          |
| Reciprocal trig        | `csc(x)`, `sec(x)`, `cot(x)`                                          |
| Inverse trig           | `asin(x)`, `acos(x)`, `atan(x)`                                       |
| Inverse recip.         | `acsc(x)`, `asec(x)`, `acot(x)`                                       |
| Hyperbolic             | `sinh(x)`, `cosh(x)`, `tanh(x)`                                       |
| Recip. hyperbolic      | `csch(x)`, `sech(x)`, `coth(x)`                                       |
| Inverse hyperbolic     | `asinh(x)`, `acosh(x)`, `atanh(x)`                                    |
| Log/Exp/Sqrt           | `ln(x)`, `log(x)`, `log2(x)`, `log10(x)`, `exp(x)`, `sqrt(x)`        |
| Complex                | `conj(x)`, `re(x)`, `im(x)`, `i`                                      |
| Rounding               | `floor(x)`, `ceil(x)`, `round(x)`                                     |
| Other                  | `abs(x)`, `sign(x)`                                                   |
| Special                | `gamma(x)`, `erf(x)`                                                  |
| Constants              | `pi`, `tau`, `e`, `phi`, `i`                                          |
| Variables              | `x`, `ans` (last computed result)                                     |

### Commands (all start with `:`)

| Command                                                        | Description                               |
|----------------------------------------------------------------|-------------------------------------------|
| `:help`                                                        | Show help message                         |
| `:quit`                                                        | Exit the REPL                             |
| `:solve <expr>`                                                | Solve linear equation `expr = 0` for x    |
| `:solveq <expr>`                                               | Solve quadratic equation `expr = 0` for x |
| `:deriv <expr> [at <x>]`                                       | Symbolic derivative, optionally evaluate  |
| `:integral <expr> from <a> to <b>`                             | Numerical definite integral from a to b   |
| `:integral <expr>`                                             | Show indefinite integral (antiderivative) |
| `:graph <expr> [from <a> to <b>]`                              | Plot function as SVG, open in browser     |
| `:mandelbrot [w h iter]`                                       | Generate Mandelbrot set SVG (default 200×200×100) |
| `:julia <re> <im> [w h iter]`                                  | Generate Julia set SVG for parameter c    |
| `:explain deriv <expr> [at <x>]`                               | Show symbolic derivative steps            |

### Calculus

- **Derivative**: symbolic differentiation with chain/product/quotient rules
- **Integral**: Simpson's adaptive quadrature; supports `inf`, `-inf` bounds
- **Antiderivative**: reverses derivative rules for simple expressions

### SVG Graphing

- Function plots: `:graph sin(x) from -5 to 5`
- Mandelbrot set: `:mandelbrot 200 200 100`
- Julia sets: `:julia -0.7 0.27 200 200 100`
- SVGs render inline in the browser

## Project layout

- `wasm/` — Rust crate compiled to WASM
- `web/` — Astro frontend with TypeScript REPL
- `dist/` — built static output
