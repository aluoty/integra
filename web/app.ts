interface EvalResponse {
  result: string;
  expr: string;
}

interface DerivResponse {
  deriv: string;
  expr: string;
}

interface SolveResponse {
  solution: string;
  expr: string;
}

interface IntegralResponse {
  result: string;
  expr: string;
  from: string;
  to: string;
}

declare const INTEGRA_API_BASE: string | undefined;

const API_BASE: string = (typeof INTEGRA_API_BASE !== 'undefined' ? INTEGRA_API_BASE : '') + '/api';

const input = document.getElementById('input') as HTMLInputElement;
const output = document.getElementById('output') as HTMLDivElement;

function addLine(text: string, cls: string = ''): void {
  const div = document.createElement('div');
  div.className = 'line' + (cls ? ' ' + cls : '');
  div.textContent = text;
  output.appendChild(div);
  output.scrollTop = output.scrollHeight;
}

function addHTML(html: string, cls: string = ''): void {
  const div = document.createElement('div');
  div.className = 'line' + (cls ? ' ' + cls : '');
  div.innerHTML = html;
  output.appendChild(div);
  output.scrollTop = output.scrollHeight;
}

async function fetchJSON<T>(url: string): Promise<T> {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.json() as Promise<T>;
}

async function fetchText(url: string): Promise<string> {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.text();
}

async function evalExpr(expr: string): Promise<void> {
  addLine('\u03BB ' + expr, 'input');
  if (!expr.trim()) return;

  if (expr.startsWith(':')) {
    await handleCommand(expr);
    return;
  }

  try {
    const data = await fetchJSON<EvalResponse>(API_BASE + '/eval?expr=' + encodeURIComponent(expr));
    addLine(data.result, 'result');
  } catch (e: unknown) {
    addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
  }
}

async function handleCommand(cmd: string): Promise<void> {
  const parts = cmd.slice(1).split(/\s+/);
  const command = parts[0];
  const rest = parts.slice(1).join(' ');

  switch (command) {
    case 'help':
      addLine('Commands:', 'info');
      addLine('  :help                    Show this help', 'info');
      addLine('  :deriv <expr> [at <x>]   Derivative', 'info');
      addLine('  :integral <expr> [from <a> to <b>]', 'info');
      addLine('  :solve <expr>            Solve linear equation', 'info');
      addLine('  :solveq <expr>           Solve quadratic equation', 'info');
      addLine('  :graph <expr> [from <a> to <b>]', 'info');
      addLine('  :mandelbrot [w h iter xmin xmax ymin ymax]', 'info');
      addLine('  :julia <cx> <cy> [w h iter xmin xmax ymin ymax]', 'info');
      addLine('  :solvec <expr>           Solve cubic equation', 'info');
      addLine('  :burningship [w h iter xmin xmax ymin ymax]', 'info');
      break;

    case 'deriv': {
      const match = rest.match(/^(.*?)\s+at\s+(.+)$/);
      let exprStr: string, atStr: string | undefined;
      if (match) {
        exprStr = match[1];
        atStr = match[2];
      } else {
        exprStr = rest;
      }
      try {
        const data = await fetchJSON<DerivResponse>(API_BASE + '/deriv?expr=' + encodeURIComponent(exprStr));
        addLine("f'(x) = " + data.deriv, 'deriv');
        if (atStr) {
          const derivAtRes = await fetchJSON<EvalResponse>(API_BASE + '/eval?expr=(' + data.deriv + ')');
          addLine("f'(" + atStr + ') = ' + derivAtRes.result, 'result');
        }
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'integral': {
      const match = rest.match(/^(.*?)\s+from\s+(.+?)\s+to\s+(.+)$/);
      if (match) {
        const exprStr = match[1];
        const fromVal = match[2];
        const toVal = match[3];
        try {
          const data = await fetchJSON<IntegralResponse>(
            API_BASE + '/integral?expr=' + encodeURIComponent(exprStr)
            + '&from=' + encodeURIComponent(fromVal)
            + '&to=' + encodeURIComponent(toVal)
          );
          addLine('\u222B f(x) dx = ' + data.result, 'result');
        } catch (e: unknown) {
          addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
        }
      } else {
        addLine('Usage: :integral <expr> from <a> to <b>', 'error');
      }
      break;
    }

    case 'solve': {
      try {
        const data = await fetchJSON<SolveResponse>(API_BASE + '/solve?expr=' + encodeURIComponent(rest));
        addLine(data.solution, 'result');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'solveq': {
      try {
        const data = await fetchJSON<SolveResponse>(API_BASE + '/solveq?expr=' + encodeURIComponent(rest));
        addLine(data.solution, 'result');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'solvec': {
      try {
        const data = await fetchJSON<SolveResponse>(API_BASE + '/solvec?expr=' + encodeURIComponent(rest));
        addLine(data.solution, 'result');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'graph': {
      const match = rest.match(/^(.*?)(?:\s+from\s+(.+?)\s+to\s+(.+))?$/);
      const exprStr = match![1];
      const fromVal = match![2] || '';
      const toVal = match![3] || '';
      try {
        const svg = await fetchText(
          API_BASE + '/graph?expr=' + encodeURIComponent(exprStr)
          + '&from=' + encodeURIComponent(fromVal)
          + '&to=' + encodeURIComponent(toVal)
        );
        addHTML('<div class="svg-container">' + svg + '</div>');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'mandelbrot': {
      const args = rest.split(/\s+/);
      const width = args[0] || '200';
      const height = args[1] || '200';
      const iter = args[2] || '100';
      const xMin = args[3] || '';
      const xMax = args[4] || '';
      const yMin = args[5] || '';
      const yMax = args[6] || '';
      try {
        let url = API_BASE + '/mandelbrot?width=' + width + '&height=' + height + '&iter=' + iter;
        if (xMin) url += '&xMin=' + xMin;
        if (xMax) url += '&xMax=' + xMax;
        if (yMin) url += '&yMin=' + yMin;
        if (yMax) url += '&yMax=' + yMax;
        const svg = await fetchText(url);
        addHTML('<div class="svg-container">' + svg + '</div>');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'julia': {
      const args = rest.split(/\s+/);
      const cx = args[0] || '-0.7';
      const cy = args[1] || '0.27015';
      const width = args[2] || '200';
      const height = args[3] || '200';
      const iter = args[4] || '100';
      const xMin = args[5] || '';
      const xMax = args[6] || '';
      const yMin = args[7] || '';
      const yMax = args[8] || '';
      try {
        let url = API_BASE + '/julia?cx=' + cx + '&cy=' + cy + '&width=' + width + '&height=' + height + '&iter=' + iter;
        if (xMin) url += '&xMin=' + xMin;
        if (xMax) url += '&xMax=' + xMax;
        if (yMin) url += '&yMin=' + yMin;
        if (yMax) url += '&yMax=' + yMax;
        const svg = await fetchText(url);
        addHTML('<div class="svg-container">' + svg + '</div>');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'burningship': {
      const args = rest.split(/\s+/);
      const width = args[0] || '200';
      const height = args[1] || '200';
      const iter = args[2] || '100';
      const xMin = args[3] || '';
      const xMax = args[4] || '';
      const yMin = args[5] || '';
      const yMax = args[6] || '';
      try {
        let url = API_BASE + '/burningship?width=' + width + '&height=' + height + '&iter=' + iter;
        if (xMin) url += '&xMin=' + xMin;
        if (xMax) url += '&xMax=' + xMax;
        if (yMin) url += '&yMin=' + yMin;
        if (yMax) url += '&yMax=' + yMax;
        const svg = await fetchText(url);
        addHTML('<div class="svg-container">' + svg + '</div>');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    default:
      addLine('Unknown command. Type :help for available commands.', 'error');
  }
}

input.addEventListener('keydown', async (e: KeyboardEvent) => {
  if (e.key === 'Enter') {
    const expr = input.value;
    input.value = '';
    await evalExpr(expr);
  }
});

document.getElementById('terminal')!.addEventListener('click', () => input.focus());
