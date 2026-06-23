import init, { process_command, graph_svg, integral_svg, integral } from './wasm/integra_wasm.js';

const output = document.getElementById('output') as HTMLDivElement;
const input = document.getElementById('input') as HTMLInputElement;

function addLine(text: string, cls = '') {
  const div = document.createElement('div');
  div.className = 'line' + (cls ? ' ' + cls : '');
  div.textContent = text;
  output.appendChild(div);
  output.scrollTop = output.scrollHeight;
}

function addHTML(html: string, cls = '') {
  const div = document.createElement('div');
  div.className = 'line' + (cls ? ' ' + cls : '');
  div.innerHTML = html;
  output.appendChild(div);
  output.scrollTop = output.scrollHeight;
}

function parseGraphCommand(cmd: string): { expr: string; xMin: number; xMax: number; yMin?: number; yMax?: number } | null {
  const m = cmd.match(/^:graph\s+(.*?)(?:\s+from\s+(-?[\d.]+)\s+to\s+(-?[\d.]+))?(?:\s+yfrom\s+(-?[\d.]+)\s+to\s+(-?[\d.]+))?$/);
  if (!m) return null;
  const expr = m[1];
  const xMin = m[2] ? parseFloat(m[2]) : -10;
  const xMax = m[3] ? parseFloat(m[3]) : 10;
  const yMin = m[4] ? parseFloat(m[4]) : undefined;
  const yMax = m[5] ? parseFloat(m[5]) : undefined;
  return { expr, xMin, xMax, yMin, yMax };
}

function parseIntegralCommand(cmd: string): { expr: string; from?: number; to?: number } | null {
  const m = cmd.match(/^:integral\s+(.*?)(?:\s+from\s+(-?[\d.]+)\s+to\s+(-?[\d.]+))?$/);
  if (!m) return null;
  return { expr: m[1], from: m[2] ? parseFloat(m[2]) : undefined, to: m[3] ? parseFloat(m[3]) : undefined };
}

async function handleInput(cmd: string) {
  addLine('\u03BB ' + cmd, 'input');
  if (!cmd.trim()) return;
  if (cmd === ':clear') { output.innerHTML = ''; return; }

  try {
    if (cmd.startsWith(':graph ')) {
      const p = parseGraphCommand(cmd);
      if (p) {
        const svg = graph_svg(p.expr, p.xMin, p.xMax, p.yMin ?? 0, p.yMax ?? 0, p.yMin === undefined);
        if (svg) addHTML('<div class="svg-container">' + svg + '</div>');
        else addLine('Error: cannot parse expression', 'error');
      }
      return;
    }

    if (cmd.startsWith(':integral ')) {
      const p = parseIntegralCommand(cmd);
      if (p && p.from !== undefined && p.to !== undefined) {
        const result = integral(p.expr, p.from, p.to);
        addLine('\u222B f(x) dx from ' + p.from + ' to ' + p.to + ' = ' + result, 'result');
        const svg = integral_svg(p.expr, p.from, p.to);
        if (svg) addHTML('<div class="svg-container">' + svg + '</div>');
      }
      return;
    }

    if (cmd.startsWith(':mandelbrot') || cmd.startsWith(':julia') || cmd.startsWith(':burningship') || cmd.startsWith(':mandel') || cmd.startsWith(':ship')) {
      const result = process_command(cmd);
      if (result.trim().startsWith('<svg')) {
        addHTML('<div class="svg-container">' + result + '</div>');
      }
      return;
    }

    const result = process_command(cmd);
    if (result === 'CLEAR') { output.innerHTML = ''; return; }
    if (result) addLine(result, 'result');
  } catch (e: unknown) {
    addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
  }
}

async function main() {
  await init();

  addLine('Integra v1.0 \u{2014} Rust + WASM', 'info');
  addLine('Type :help for commands.', 'info');

  input.addEventListener('keydown', (e: KeyboardEvent) => {
    if (e.key === 'Enter') {
      const val = input.value;
      input.value = '';
      handleInput(val);
    }
  });

  document.getElementById('terminal')!.addEventListener('click', () => input.focus());
}

main();
