# integra

A terminal-based REPL calculator with arithmetic, trigonometry, calculus, and algebra solving — built in Haskell.

## Quick start

```bash
cabal run
```

You'll see the `λ>` prompt. Type `:help` for all commands.

## Features

### Expressions
| Category           | Examples                                                        |
|--------------------|-----------------------------------------------------------------|
| Arithmetic         | `2 + 3`, `4 * 5`, `10 / 2`, `2^3`                              |
| Trig               | `sin(x)`, `cos(x)`, `tan(x)`                                    |
| Reciprocal trig    | `csc(x)`, `sec(x)`, `cot(x)`                                    |
| Inverse trig       | `asin(x)`, `acos(x)`, `atan(x)`                                 |
| Inverse recip.     | `acsc(x)`, `asec(x)`, `acot(x)`                                 |
| Hyperbolic         | `sinh(x)`, `cosh(x)`, `tanh(x)`                                 |
| Recip. hyperbolic  | `csch(x)`, `sech(x)`, `coth(x)`                                 |
| Log/Exp/Sqrt       | `log(x)`, `log2(x)`, `log10(x)`, `exp(x)`, `sqrt(x)`           |
| Rounding           | `floor(x)`, `ceil(x)`, `round(x)`                               |
| Other              | `abs(x)`, `sign(x)`                                             |
| Special            | `gamma(x)`, `erf(x)`                                            |
| Constants          | `pi`, `tau`, `e`, `phi`                                         |
| Variables          | `x`, `ans` (last computed result)                               |

Operator precedence: `^` (right-assoc) > `*` `/` > `+` `-`.

### Commands (all start with `:`)

| Command                                               | Description                               |
|-------------------------------------------------------|-------------------------------------------|
| `:help`                                               | Show help message                         |
| `:about`                                              | About Integra                             |
| `:clear`                                              | Clear the screen                          |
| `:quit`                                               | Exit the REPL                             |
| `:solve <expr>`                                       | Solve linear equation `expr = 0` for x    |
| `:solveq <expr>`                                      | Solve quadratic equation `expr = 0` for x |
| `:deriv <expr> at <x>`                                | Numerical 1st derivative at x             |
| `:deriv2 <expr> at <x>`                               | Numerical 2nd derivative at x             |
| `:derivn <expr> order <n> at <x>`                     | Numerical nth derivative at x             |
| `:integral <expr> from <a> to <b>`                    | Numerical definite integral from a to b   |
| `:limit <expr> as x -> <a>`                           | Numerical limit as x approaches a         |
| `:taylor <expr> at <a> order <n>`                     | Taylor series evaluated at x = 0          |

### Calculus (numerical)
- **Derivative**: central difference method with `h = 1e-8` / `h = 1e-5` for 2nd
- **Integral**: Simpson's rule with 1000 subdivisions
- **Limit**: one-sided evaluation with `h = 1e-10`

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
