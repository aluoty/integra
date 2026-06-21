# integra

A terminal-based REPL calculator with arithmetic, trigonometry, calculus, and algebra solving — built in Haskell.

## Quick start

```bash
cabal run
```

You'll see the `>integra` prompt. Type `:help` for all commands.

## Features

### Expressions
| Category       | Examples                                              |
|----------------|-------------------------------------------------------|
| Arithmetic     | `2 + 3`, `4 * 5`, `10 / 2`, `2^3`                     |
| Trig           | `sin(x)`, `cos(x)`, `tan(x)`                          |
| Inverse trig   | `asin(x)`, `acos(x)`, `atan(x)`                       |
| Hyperbolic     | `sinh(x)`, `cosh(x)`, `tanh(x)`                       |
| Log / Exp      | `log(x)`, `exp(x)`, `sqrt(x)`                         |
| Rounding       | `floor(x)`, `ceil(x)`, `round(x)`                     |
| Absolute value | `abs(x)`                                              |
| Constants      | `pi`, `e`                                             |
| Variables      | `x`, `ans` (last computed result)                     |

Operator precedence: `^` (right-assoc) > `*` `/` > `+` `-`.

### Commands (all start with `:`)

| Command                                               | Description                               |
|-------------------------------------------------------|-------------------------------------------|
| `:help`                                               | Show help message                         |
| `:quit`                                               | Exit the REPL                             |
| `:solve <expr>`                                       | Solve linear equation `expr = 0` for x    |
| `:solveq <expr>`                                      | Solve quadratic equation `expr = 0` for x |
| `:deriv <expr> at <x>`                                | Numerical derivative of `expr` at x       |
| `:integral <expr> from <a> to <b>`                    | Numerical definite integral from a to b   |

### Algebra solvers
- `:solve` extracts coefficients of `a*x + b` and returns `x = -b/a`
- `:solveq` extracts coefficients of `a*x² + b*x + c` and returns real or complex roots

### Calculus (numerical)
- **Derivative**: central difference method with `h = 1e-8`
- **Integral**: Simpson's rule with 1000 subdivisions

### REPL features
- Line editing with history (up/down arrows)
- Tab completion for commands (`:h` + Tab → `:help`)
- `ans` variable stores the last result
- Error messages for invalid input (no crashes)

## Clean

```bash
cabal clean
```
