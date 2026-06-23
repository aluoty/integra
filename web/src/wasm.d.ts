declare module './wasm/integra_wasm.js' {
  export function init(): Promise<void>;
  export function process_command(cmd: string): string;
  export function evaluate(expr: string): string;
  export function solve_linear(expr: string): string;
  export function solve_quadratic(expr: string): string;
  export function solve_cubic(expr: string): string;
  export function symbolic_deriv(expr: string): string;
  export function derivative_at(expr: string, x: number): string;
  export function derivative2_at(expr: string, x: number): string;
  export function derivative_n_at(expr: string, n: number, x: number): string;
  export function integral(expr: string, from: number, to: number): string;
  export function limit_at(expr: string, x: number): string;
  export function taylor_at(expr: string, a: number, order: number): string;
  export function graph_svg(expr: string, x_min: number, x_max: number, y_min: number, y_max: number, auto_y: boolean): string;
  export function integral_svg(expr: string, a: number, b: number): string;
  export function mandelbrot_svg(w: number, h: number, max_iter: number, x_min: number, x_max: number, y_min: number, y_max: number): string;
  export function julia_svg(cx: number, cy: number, w: number, h: number, max_iter: number, x_min: number, x_max: number, y_min: number, y_max: number): string;
  export function burning_ship_svg(w: number, h: number, max_iter: number, x_min: number, x_max: number, y_min: number, y_max: number): string;
  export function antideriv(expr: string): string;
  export function init_panic_hook(): void;
}
