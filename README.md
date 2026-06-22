# integra

A terminal-based REPL calculator with complex numbers, symbolic differentiation,
SVG graphing, fractals, calculus, and algebra solving — built in Haskell.

## Quick start

```bash
cabal run
```

You'll see the `λ` prompt. Type `:help` for all commands.

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

Operator precedence: `==` `!=` `>` `>=` `<` `<=` (lowest) > `+` `-` > `*` `/` > `^` (right-assoc) > implicit multiplication.

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
| `:explain integral <expr>`                                     | Show antiderivative rules                 |
| `:explain solve <expr>`                                        | Show solving steps                        |

### Calculus

- **Derivative**: symbolic differentiation with chain/product/quotient rules
- **Integral**: Simpson's adaptive quadrature; supports `inf`, `-inf` bounds (mapped to ±1e6)
- **Antiderivative**: reverses derivative rules for simple expressions

### Complex numbers

- `i` is the imaginary unit. All arithmetic, trig, hyperbolic, log, exp, sqrt,
  and power operations work on `Complex Double`.
- Display: `3+4i`, `5`, `-2i`, `∞`, `undefined`.
- The solver returns complex roots automatically.

### Step-by-step explanations

- `:explain deriv <expr>` — symbolic differentiation with rule breakdown
- `:explain integral <expr>` — antiderivative rules shown step by step
- `:explain solve <expr>` — linear equation solving steps

### SVG Graphing

- Function plots: `:graph sin(x) from -5 to 5` (auto-scaled y-axis, grid lines, axis labels)
- Mandelbrot set: `:mandelbrot 200 200 100` (smooth coloring, HSL palette)
- Julia sets: `:julia -0.7 0.27 200 200 100`
- SVGs are saved to `/tmp/` and opened in your default browser.

### Multiple solutions

- `8^(1/3)` shows all 3 cube roots.
- `(-1)^(1/2)` shows both `i` and `-i`.
- Works for any constant base `a^(1/n)` with integer `n > 1`.

### REPL features

- Colored output (ANSI terminal)
- Line editing with history (up/down arrows) via `haskeline`
- `ans` variable stores the last result
- `niceShow` formatting — `5.0` → `5`, `NaN` → `undefined`, `Infinity` → `∞`

### Comparison operators

- `3 > 2` returns `1`, `1 < 0` returns `0`
- `1 == 1` returns `1`, `1 != 2` returns `1`
- Comparisons compare the real part only (imaginary part ignored)

## Build

```bash
cabal build
cabal run
```

## Clean

```bash
cabal clean
```
