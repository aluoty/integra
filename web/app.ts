interface EvalResponse {
  result: string;
  expr: string;
}

interface DerivResponse {
  deriv: string;
  result: string;
  expr: string;
  at: string;
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

interface ExplainResponse {
  text: string;
  type: string;
}

interface AboutResponse {
  text: string;
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
      addLine('  :help                     Show this help', 'info');
      addLine('  :about                    About Integra', 'info');
      addLine('  :clear                    Clear output', 'info');
      addLine('  :deriv <expr> [at <x>]    Derivative', 'info');
      addLine('  :deriv2 <expr> at <x>     Numerical 2nd derivative', 'info');
      addLine('  :derivn <expr> order <n> at <x>', 'info');
      addLine('  :integral <expr> [from <a> to <b>]', 'info');
      addLine('  :limit <expr> as <x>      Numerical limit', 'info');
      addLine('  :taylor <expr> at <a> order <n>', 'info');
      addLine('  :solve <expr>             Solve linear equation', 'info');
      addLine('  :solveq <expr>            Solve quadratic equation', 'info');
      addLine('  :solvec <expr>            Solve cubic equation', 'info');
      addLine('  :graph <expr> [from <a> to <b>] [yfrom <ya> to <yb>]', 'info');
      addLine('  :mandelbrot [w h iter xmin xmax ymin ymax]', 'info');
      addLine('  :julia <cx> <cy> [w h iter xmin xmax ymin ymax]', 'info');
      addLine('  :burningship [w h iter xmin xmax ymin ymax]', 'info');
      addLine('  :explain deriv|integral|solve <expr>', 'info');
      addLine('  :quit                     (close the tab)', 'info');
      break;

    case 'about':
      try {
        const data = await fetchJSON<AboutResponse>(API_BASE + '/about');
        addLine(data.text, 'info');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;

    case 'clear':
      output.innerHTML = '';
      break;

    case 'quit':
      addLine('Close this tab to quit.', 'info');
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
        let url = API_BASE + '/deriv?expr=' + encodeURIComponent(exprStr);
        if (atStr) url += '&at=' + encodeURIComponent(atStr);
        const data = await fetchJSON<DerivResponse>(url);
        addLine("f'(x) = " + data.deriv, 'deriv');
        if (data.result) {
          addLine("f'(" + atStr + ') = ' + data.result, 'result');
        }
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'deriv2': {
      const match = rest.match(/^(.*?)\s+at\s+(.+)$/);
      if (!match) {
        addLine('Usage: :deriv2 <expr> at <x>', 'error');
        break;
      }
      const exprStr = match[1];
      const atStr = match[2];
      try {
        const data = await fetchJSON<EvalResponse>(
          API_BASE + '/deriv2?expr=' + encodeURIComponent(exprStr)
          + '&at=' + encodeURIComponent(atStr)
        );
        addLine("f''(" + atStr + ') = ' + data.result, 'result');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'derivn': {
      const orderMatch = rest.match(/^(.*?)\s+order\s+(.+?)\s+at\s+(.+)$/);
      if (!orderMatch) {
        addLine('Usage: :derivn <expr> order <n> at <x>', 'error');
        break;
      }
      const exprStr = orderMatch[1];
      const orderStr = orderMatch[2];
      const atStr = orderMatch[3];
      try {
        const data = await fetchJSON<EvalResponse>(
          API_BASE + '/derivn?expr=' + encodeURIComponent(exprStr)
          + '&order=' + encodeURIComponent(orderStr)
          + '&at=' + encodeURIComponent(atStr)
        );
        addLine("f^(" + orderStr + ")(" + atStr + ') = ' + data.result, 'result');
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
        try {
          const data = await fetchJSON<EvalResponse>(
            API_BASE + '/antideriv?expr=' + encodeURIComponent(rest)
          );
          addLine('\u222B f(x) dx = ' + data.result, 'result');
        } catch (e: unknown) {
          addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
        }
      }
      break;
    }

    case 'limit': {
      const match = rest.match(/^(.*?)\s+as\s+(.+)$/);
      if (!match) {
        addLine('Usage: :limit <expr> as <x>', 'error');
        break;
      }
      const exprStr = match[1];
      const asStr = match[2];
      try {
        const data = await fetchJSON<EvalResponse>(
          API_BASE + '/limit?expr=' + encodeURIComponent(exprStr)
          + '&as=' + encodeURIComponent(asStr)
        );
        addLine('lim f(x) as x \u2192 ' + asStr + ' = ' + data.result, 'result');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'taylor': {
      const match = rest.match(/^(.*?)\s+at\s+(.+?)\s+order\s+(.+)$/);
      if (!match) {
        addLine('Usage: :taylor <expr> at <a> order <n>', 'error');
        break;
      }
      const exprStr = match[1];
      const atStr = match[2];
      const orderStr = match[3];
      try {
        const data = await fetchJSON<EvalResponse>(
          API_BASE + '/taylor?expr=' + encodeURIComponent(exprStr)
          + '&at=' + encodeURIComponent(atStr)
          + '&order=' + encodeURIComponent(orderStr)
        );
        addLine('T_' + orderStr + '(0) \u2248 ' + data.result, 'result');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
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

    case 'explain': {
      const explainMatch = rest.match(/^(deriv|integral|solve)\s+(.+)$/);
      if (!explainMatch) {
        addLine('Usage: :explain deriv|integral|solve <expr>', 'error');
        break;
      }
      const explainType = explainMatch[1];
      const exprStr = explainMatch[2];
      try {
        const data = await fetchJSON<ExplainResponse>(
          API_BASE + '/explain?type=' + encodeURIComponent(explainType)
          + '&expr=' + encodeURIComponent(exprStr)
        );
        addLine(data.text, 'info');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'graph': {
      const match = rest.match(/^(.*?)(?:\s+from\s+(.+?)\s+to\s+(.+?))?(?:\s+yfrom\s+(.+?)\s+to\s+(.+))?$/);
      const exprStr = match![1];
      const fromVal = match![2] || '';
      const toVal = match![3] || '';
      const yMinVal = match![4] || '';
      const yMaxVal = match![5] || '';
      try {
        let url = API_BASE + '/graph?expr=' + encodeURIComponent(exprStr);
        if (fromVal) url += '&from=' + encodeURIComponent(fromVal);
        if (toVal) url += '&to=' + encodeURIComponent(toVal);
        if (yMinVal) url += '&yMin=' + encodeURIComponent(yMinVal);
        if (yMaxVal) url += '&yMax=' + encodeURIComponent(yMaxVal);
        const svg = await fetchText(url);
        addHTML('<div class="svg-container">' + svg + '</div>');
      } catch (e: unknown) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
      }
      break;
    }

    case 'mandelbrot': {
      const args = rest.split(/\s+/);
      const width = args[0] || '400';
      const height = args[1] || '400';
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
      const width = args[2] || '400';
      const height = args[3] || '400';
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
      const width = args[0] || '400';
      const height = args[1] || '400';
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
