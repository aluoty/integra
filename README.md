# integra

A terminal-based REPL calculator with complex numbers, symbolic differentiation,
SVG graphing, fractals, calculus, and algebra solving — built in Haskell.

## Quick start

```bash
cabal run
```

You'll see the `λ>` prompt. Type `:help` for all commands.

## Features

### Expressions

| Category               | Examples                                                              |
|------------------------|-----------------------------------------------------------------------|
| Arithmetic             | `2 + 3`, `4 * 5`, `10 / 2`, `2^3`                                    |
| Trig                   | `sin(x)`, `cos(x)`, `tan(x)`                                          |
| Reciprocal trig        | `csc(x)`, `sec(x)`, `cot(x)`                                          |
| Inverse trig           | `asin(x)`, `acos(x)`, `atan(x)`                                       |
| Inverse recip.         | `acsc(x)`, `asec(x)`, `acot(x)`                                       |
| Hyperbolic             | `sinh(x)`, `cosh(x)`, `tanh(x)`                                       |
| Recip. hyperbolic      | `csch(x)`, `sech(x)`, `coth(x)`                                       |
| Inverse hyperbolic     | `asinh(x)`, `acosh(x)`, `atanh(x)`                                    |
| Log/Exp/Sqrt           | `log(x)`, `log2(x)`, `log10(x)`, `exp(x)`, `sqrt(x)`                 |
| Complex                | `conj(x)`, `re(x)`, `im(x)`, `i`                                      |
| Rounding               | `floor(x)`, `ceil(x)`, `round(x)`                                     |
| Other                  | `abs(x)`, `sign(x)`                                                   |
| Special                | `gamma(x)`, `erf(x)`                                                  |
| Constants              | `pi`, `tau`, `e`, `phi`, `i`                                          |
| Variables              | `x`, `ans` (last computed result)                                     |

Operator precedence: `^` (right-assoc) > `*` `/` > `+` `-`.

### Commands (all start with `:`)

| Command                                                        | Description                               |
|----------------------------------------------------------------|-------------------------------------------|
| `:help`                                                        | Show help message                         |
| `:about`                                                       | About Integra                             |
| `:clear`                                                       | Clear the screen                          |
| `:quit`                                                        | Exit the REPL                             |
| `:solve <expr>`                                                | Solve linear equation `expr = 0` for x    |
| `:solveq <expr>`                                               | Solve quadratic equation `expr = 0` for x |
| `:deriv <expr> at <x>`                                         | Numerical 1st derivative at x             |
| `:deriv2 <expr> at <x>`                                        | Numerical 2nd derivative at x             |
| `:derivn <expr> order <n> at <x>`                              | Numerical nth derivative at x             |
| `:integral <expr> from <a> to <b>`                             | Numerical definite integral from a to b   |
| `:integral <expr>`                                             | Show indefinite integral (antiderivative) |
| `:limit <expr> as x -> <a>`                                    | Numerical limit as x approaches a         |
| `:taylor <expr> at <a> order <n>`                              | Taylor series evaluated at x = 0          |
| `:graph <expr> from <a> to <b>`                                | Plot function as SVG, open in browser     |
| `:mandelbrot`                                                  | Generate Mandelbrot set SVG               |
| `:julia <re> <im>`                                             | Generate Julia set SVG for parameter c    |
| `:explain deriv <expr>`                                        | Show symbolic derivative steps            |
| `:explain deriv <expr> at <x>`                                 | Show derivative steps & evaluate at x     |
| `:explain integral <expr>`                                     | Show antiderivative rules                 |
| `:explain solve <expr>`                                        | Show solving steps                        |

### Calculus (numerical)

- **Derivative**: central difference method with `h = 1e-8` / `h = 1e-5` for 2nd
- **Integral**: Simpson's rule with 1000 subdivisions; supports `inf`, `-inf` bounds
- **Limit**: one-sided evaluation with `h = 1e-10`

### Complex numbers

- `i` is the imaginary unit. All arithmetic, trig, hyperbolic, log, exp, sqrt,
  and power operations work on complex numbers.
- Display: `3 + 4i`, `5`, `-2i`, `∞`, `undefined`.
- The solver returns complex roots automatically.

### Step-by-step explanations

- Symbolic differentiation with chain rule, product rule, quotient rule, etc.
- Shows the rule applied at each step with the intermediate expression.

### SVG Graphing

- Function plots: `:graph sin(x) from -5 to 5`
- Mandelbrot set: `:mandelbrot` (160×120, 100 iterations)
- Julia sets: `:julia -0.7 0.27` (renders 160×120 SVG)
- SVGs are saved to `/tmp/` and opened in your default browser.

### Multiple solutions

- `8^(1/3)` shows all 3 cube roots.
- `(-1)^(1/2)` shows both `i` and `-i`.
- Works for any constant base `a^(1/n)` with integer `n > 1`.

### REPL features

- Colored output (ANSI terminal)
- Line editing with history (up/down arrows) via `haskeline`
- Tab completion for commands
- `ans` variable stores the last result
- `niceShow` formatting — `5.0` → `5`, `NaN` → `undefined`, `Infinity` → `∞`

## Clean

```bash
cabal clean
```
