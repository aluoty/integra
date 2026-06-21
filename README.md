# integra

A terminal-based REPL calculator with arithmetic, trigonometry, calculus, and algebra solving — built in Haskell.

## Quick start

```bash
cabal run
```

## Features

### Expressions
| Category     | Examples                          |
|--------------|-----------------------------------|
| Arithmetic   | `2 + 3`, `4 * 5`, `10 / 2`, `2^3` |
| Trigonometry | `sin(x)`, `cos(x)`, `tan(x)`      |
| Log / Exp    | `log(x)`, `exp(x)`, `sqrt(x)`     |
| Constants    | `pi`, `e`                         |
| Variable     | `x`                               |

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

## Clean

```bash
cabal clean
```
