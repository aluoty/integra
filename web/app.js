"use strict";
const API_BASE = (typeof INTEGRA_API_BASE !== 'undefined' ? INTEGRA_API_BASE : '') + '/api';
const input = document.getElementById('input');
const output = document.getElementById('output');
const FUNCS = new Set(['sin', 'cos', 'tan', 'ln', 'log', 'log2', 'log10', 'exp', 'sqrt', 'abs', 'floor', 'ceil', 'round', 're', 'im', 'conj', 'sign', 'gamma', 'erf', 'sinh', 'cosh', 'tanh', 'asin', 'acos', 'atan', 'csc', 'sec', 'cot']);
function tokenize(s) {
    const t = [];
    let i = 0;
    while (i < s.length) {
        if (s[i] === ' ') {
            i++;
            continue;
        }
        if ('+-*/^()'.includes(s[i])) {
            if (s[i] === '(')
                t.push({ t: 'lp' });
            else if (s[i] === ')')
                t.push({ t: 'rp' });
            else if (s[i] === ',')
                t.push({ t: 'comma' });
            else
                t.push({ t: 'op', v: s[i] });
            i++;
            continue;
        }
        if (s[i] === '.' || (s[i] >= '0' && s[i] <= '9')) {
            let v = '';
            while (i < s.length && (s[i] >= '0' && s[i] <= '9' || s[i] === '.')) {
                v += s[i];
                i++;
            }
            t.push({ t: 'num', v: parseFloat(v) });
            continue;
        }
        if ((s[i] >= 'a' && s[i] <= 'z') || (s[i] >= 'A' && s[i] <= 'Z')) {
            let id = '';
            while (i < s.length && ((s[i] >= 'a' && s[i] <= 'z') || (s[i] >= 'A' && s[i] <= 'Z') || (s[i] >= '0' && s[i] <= '9'))) {
                id += s[i];
                i++;
            }
            if (id === 'pi')
                t.push({ t: 'num', v: Math.PI });
            else if (id === 'e')
                t.push({ t: 'num', v: Math.E });
            else if (id === 'i' || id === 'phi' || id === 'tau')
                t.push({ t: 'num', v: 0 });
            else
                t.push({ t: 'id', v: id });
            continue;
        }
        return []; // error
    }
    return t;
}
function parseExpr(toks, i) { return parseAddSub(toks, i); }
function parseAddSub(toks, i) {
    let [l, i2] = parseMulDiv(toks, i);
    while (i2 < toks.length) {
        const tk = toks[i2];
        if (tk.t !== 'op' || (tk.v !== '+' && tk.v !== '-'))
            break;
        const [r, i3] = parseMulDiv(toks, i2 + 1);
        l = { n: 'bin', op: tk.v, a: l, b: r };
        i2 = i3;
    }
    return [l, i2];
}
function parseMulDiv(toks, i) {
    let [l, i2] = parsePower(toks, i);
    while (i2 < toks.length) {
        const tk = toks[i2];
        if (tk.t === 'op' && (tk.v === '*' || tk.v === '/')) {
            const [r, i3] = parsePower(toks, i2 + 1);
            l = { n: 'bin', op: tk.v, a: l, b: r };
            i2 = i3;
        }
        else if (tk.t === 'num' || tk.t === 'id' || tk.t === 'lp') {
            const [r, i3] = parsePower(toks, i2);
            l = { n: 'bin', op: '*', a: l, b: r };
            i2 = i3;
        }
        else
            break;
    }
    return [l, i2];
}
function parsePower(toks, i) {
    let [l, i2] = parseUnary(toks, i);
    if (i2 < toks.length) {
        const tk = toks[i2];
        if (tk.t === 'op' && tk.v === '^') {
            const [r, i3] = parsePower(toks, i2 + 1);
            return [{ n: 'bin', op: '^', a: l, b: r }, i3];
        }
    }
    return [l, i2];
}
function parseUnary(toks, i) {
    if (i < toks.length && toks[i].t === 'op' && toks[i].v === '-') {
        const [a, i2] = parseUnary(toks, i + 1);
        return [{ n: 'un', op: '-', a }, i2];
    }
    return parseAtom(toks, i);
}
function parseAtom(toks, i) {
    if (i >= toks.length)
        return [{ n: 'num', v: 0 }, i];
    const tk = toks[i];
    if (tk.t === 'num') {
        if (i + 1 < toks.length && toks[i + 1].t === 'id') {
            // implicit: 2x
            const [r, i2] = parseAtom(toks, i + 1);
            return [{ n: 'bin', op: '*', a: { n: 'num', v: tk.v }, b: r }, i2];
        }
        return [{ n: 'num', v: tk.v }, i + 1];
    }
    if (tk.t === 'id') {
        if (FUNCS.has(tk.v)) {
            if (i + 1 < toks.length && toks[i + 1].t === 'lp') {
                const [a, i2] = parseParen(toks, i + 1);
                return [{ n: 'call', fn: tk.v, a }, i2];
            }
            // implicit parens: sin x
            const [a, i2] = parseAtom(toks, i + 1);
            return [{ n: 'call', fn: tk.v, a }, i2];
        }
        return [{ n: 'var', v: tk.v }, i + 1];
    }
    if (tk.t === 'lp')
        return parseParen(toks, i);
    return [{ n: 'num', v: 0 }, i + 1];
}
function parseParen(toks, i) {
    if (toks[i].t === 'lp') {
        const [a, i2] = parseExpr(toks, i + 1);
        if (i2 < toks.length && toks[i2].t === 'rp')
            return [a, i2 + 1];
        return [a, i2];
    }
    return parseAtom(toks, i);
}
function evalAST(ast, x) {
    switch (ast.n) {
        case 'num': return ast.v;
        case 'var': return x;
        case 'un': {
            const v = evalAST(ast.a, x);
            if (ast.op === '-')
                return -v;
            return v;
        }
        case 'bin': {
            const a = evalAST(ast.a, x), b = evalAST(ast.b, x);
            switch (ast.op) {
                case '+': return a + b;
                case '-': return a - b;
                case '*': return a * b;
                case '/': return a / b;
                case '^': return Math.pow(a, b);
                default: return NaN;
            }
        }
        case 'call': {
            const a = evalAST(ast.a, x);
            switch (ast.fn) {
                case 'sin': return Math.sin(a);
                case 'cos': return Math.cos(a);
                case 'tan': return Math.tan(a);
                case 'ln': return Math.log(a);
                case 'log': return Math.log(a);
                case 'log2': return Math.log2(a);
                case 'log10': return Math.log10(a);
                case 'exp': return Math.exp(a);
                case 'sqrt': return Math.sqrt(a);
                case 'abs': return Math.abs(a);
                case 'floor': return Math.floor(a);
                case 'ceil': return Math.ceil(a);
                case 'round': return Math.round(a);
                case 're': return a;
                case 'im': return 0;
                case 'sign': return a > 0 ? 1 : a < 0 ? -1 : 0;
                case 'sinh': return Math.sinh(a);
                case 'cosh': return Math.cosh(a);
                case 'tanh': return Math.tanh(a);
                case 'asin': return Math.asin(a);
                case 'acos': return Math.acos(a);
                case 'atan': return Math.atan(a);
                case 'csc': return 1 / Math.sin(a);
                case 'sec': return 1 / Math.cos(a);
                case 'cot': return 1 / Math.tan(a);
                default: return NaN;
            }
        }
    }
}
function clientEval(expr) {
    try {
        const toks = tokenize(expr);
        if (toks.length === 0)
            return null;
        const [ast] = parseExpr(toks, 0);
        const result = evalAST(ast, 0);
        if (!isFinite(result))
            return null;
        if (Number.isInteger(result) && Math.abs(result) < 1e15)
            return result.toString();
        return parseFloat(result.toFixed(10)).toString();
    }
    catch {
        return null;
    }
}
// ─── UI helpers ──────────────────────────────────────────
function addLine(text, cls = '') {
    const div = document.createElement('div');
    div.className = 'line' + (cls ? ' ' + cls : '');
    div.textContent = text;
    output.appendChild(div);
    output.scrollTop = output.scrollHeight;
}
function addHTML(html, cls = '') {
    const div = document.createElement('div');
    div.className = 'line' + (cls ? ' ' + cls : '');
    div.innerHTML = html;
    output.appendChild(div);
    output.scrollTop = output.scrollHeight;
}
async function fetchJSON(url) {
    const res = await fetch(url);
    if (!res.ok)
        throw new Error(`HTTP ${res.status}`);
    return res.json();
}
async function fetchText(url) {
    const res = await fetch(url);
    if (!res.ok)
        throw new Error(`HTTP ${res.status}`);
    return res.text();
}
async function tryAPIFallback(url) {
    try {
        return await fetchJSON(url);
    }
    catch {
        return null;
    }
}
// ─── Command dispatch ────────────────────────────────────
async function evalExpr(expr) {
    addLine('\u03BB ' + expr, 'input');
    if (!expr.trim())
        return;
    if (expr.startsWith(':')) {
        await handleCommand(expr);
        return;
    }
    // try client-side first
    const local = clientEval(expr);
    if (local !== null) {
        addLine(local, 'result');
        return;
    }
    // fallback to API
    try {
        const data = await fetchJSON(API_BASE + '/eval?expr=' + encodeURIComponent(expr));
        addLine(data.result, 'result');
    }
    catch (e) {
        addLine('Error: ' + (e instanceof Error ? e.message : String(e)), 'error');
    }
}
async function handleCommand(cmd) {
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
            break;
        case 'about': {
            const data = await tryAPIFallback(API_BASE + '/about');
            if (data)
                addLine(data.text, 'info');
            else
                addLine('Integra v1.0 — Web Calculator', 'info');
            break;
        }
        case 'clear':
            output.innerHTML = '';
            break;
        case 'quit':
            addLine('Close this tab to quit.', 'info');
            break;
        case 'deriv': {
            const m = rest.match(/^(.*?)\s+at\s+(.+)$/);
            const exprStr = m ? m[1] : rest;
            const atStr = m ? m[2] : undefined;
            let url = API_BASE + '/deriv?expr=' + encodeURIComponent(exprStr);
            if (atStr)
                url += '&at=' + encodeURIComponent(atStr);
            try {
                const data = await fetchJSON(url);
                addLine("f'(x) = " + data.deriv, 'deriv');
                if (data.result)
                    addLine("f'(" + atStr + ') = ' + data.result, 'result');
            }
            catch {
                addLine('Derivative requires a backend server. Run `cabal run integra-web` locally.', 'error');
            }
            break;
        }
        case 'deriv2': {
            const m = rest.match(/^(.*?)\s+at\s+(.+)$/);
            if (!m) {
                addLine('Usage: :deriv2 <expr> at <x>', 'error');
                break;
            }
            try {
                const data = await fetchJSON(API_BASE + '/deriv2?expr=' + encodeURIComponent(m[1]) + '&at=' + encodeURIComponent(m[2]));
                addLine("f''(" + m[2] + ') = ' + data.result, 'result');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'derivn': {
            const m = rest.match(/^(.*?)\s+order\s+(.+?)\s+at\s+(.+)$/);
            if (!m) {
                addLine('Usage: :derivn <expr> order <n> at <x>', 'error');
                break;
            }
            try {
                const data = await fetchJSON(API_BASE + '/derivn?expr=' + encodeURIComponent(m[1]) + '&order=' + encodeURIComponent(m[2]) + '&at=' + encodeURIComponent(m[3]));
                addLine("f^(" + m[2] + ")(" + m[3] + ') = ' + data.result, 'result');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'integral': {
            const m = rest.match(/^(.*?)\s+from\s+(.+?)\s+to\s+(.+)$/);
            if (m) {
                try {
                    const data = await fetchJSON(API_BASE + '/integral?expr=' + encodeURIComponent(m[1]) + '&from=' + encodeURIComponent(m[2]) + '&to=' + encodeURIComponent(m[3]));
                    addLine('\u222B f(x) dx = ' + data.result, 'result');
                }
                catch {
                    addLine('Requires a backend server.', 'error');
                }
            }
            else {
                try {
                    const data = await fetchJSON(API_BASE + '/antideriv?expr=' + encodeURIComponent(rest));
                    addLine('\u222B f(x) dx = ' + data.result, 'result');
                }
                catch {
                    addLine('Requires a backend server.', 'error');
                }
            }
            break;
        }
        case 'limit': {
            const m = rest.match(/^(.*?)\s+as\s+(.+)$/);
            if (!m) {
                addLine('Usage: :limit <expr> as <x>', 'error');
                break;
            }
            try {
                const data = await fetchJSON(API_BASE + '/limit?expr=' + encodeURIComponent(m[1]) + '&as=' + encodeURIComponent(m[2]));
                addLine('lim f(x) as x \u2192 ' + m[2] + ' = ' + data.result, 'result');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'taylor': {
            const m = rest.match(/^(.*?)\s+at\s+(.+?)\s+order\s+(.+)$/);
            if (!m) {
                addLine('Usage: :taylor <expr> at <a> order <n>', 'error');
                break;
            }
            try {
                const data = await fetchJSON(API_BASE + '/taylor?expr=' + encodeURIComponent(m[1]) + '&at=' + encodeURIComponent(m[2]) + '&order=' + encodeURIComponent(m[3]));
                addLine('T_' + m[3] + '(0) \u2248 ' + data.result, 'result');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'solve':
        case 'solveq':
        case 'solvec': {
            const ep = command === 'solve' ? 'solve' : command === 'solveq' ? 'solveq' : 'solvec';
            try {
                const data = await fetchJSON(API_BASE + '/' + ep + '?expr=' + encodeURIComponent(rest));
                addLine(data.solution, 'result');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'explain': {
            const m = rest.match(/^(deriv|integral|solve)\s+(.+)$/);
            if (!m) {
                addLine('Usage: :explain deriv|integral|solve <expr>', 'error');
                break;
            }
            try {
                const data = await fetchJSON(API_BASE + '/explain?type=' + encodeURIComponent(m[1]) + '&expr=' + encodeURIComponent(m[2]));
                addLine(data.text, 'info');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'graph': {
            const m = rest.match(/^(.*?)(?:\s+from\s+(.+?)\s+to\s+(.+?))?(?:\s+yfrom\s+(.+?)\s+to\s+(.+))?$/);
            const exprStr = m[1], fromVal = m[2] || '', toVal = m[3] || '', yMinVal = m[4] || '', yMaxVal = m[5] || '';
            try {
                let url = API_BASE + '/graph?expr=' + encodeURIComponent(exprStr);
                if (fromVal)
                    url += '&from=' + encodeURIComponent(fromVal);
                if (toVal)
                    url += '&to=' + encodeURIComponent(toVal);
                if (yMinVal)
                    url += '&yMin=' + encodeURIComponent(yMinVal);
                if (yMaxVal)
                    url += '&yMax=' + encodeURIComponent(yMaxVal);
                const svg = await fetchText(url);
                addHTML('<div class="svg-container">' + svg + '</div>');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        case 'mandelbrot':
        case 'julia':
        case 'burningship': {
            const args = rest.split(/\s+/);
            try {
                let url;
                if (command === 'julia') {
                    const cx = args[0] || '-0.7', cy = args[1] || '0.27015';
                    url = API_BASE + '/julia?cx=' + cx + '&cy=' + cy;
                    url += '&width=' + (args[2] || '400') + '&height=' + (args[3] || '400') + '&iter=' + (args[4] || '100');
                    if (args[5])
                        url += '&xMin=' + args[5];
                    if (args[6])
                        url += '&xMax=' + args[6];
                    if (args[7])
                        url += '&yMin=' + args[7];
                    if (args[8])
                        url += '&yMax=' + args[8];
                }
                else {
                    url = API_BASE + '/' + command + '?width=' + (args[0] || '400') + '&height=' + (args[1] || '400') + '&iter=' + (args[2] || '100');
                    if (args[3])
                        url += '&xMin=' + args[3];
                    if (args[4])
                        url += '&xMax=' + args[4];
                    if (args[5])
                        url += '&yMin=' + args[5];
                    if (args[6])
                        url += '&yMax=' + args[6];
                }
                const svg = await fetchText(url);
                addHTML('<div class="svg-container">' + svg + '</div>');
            }
            catch {
                addLine('Requires a backend server.', 'error');
            }
            break;
        }
        default:
            addLine('Unknown command. Type :help for available commands.', 'error');
    }
}
input.addEventListener('keydown', async (e) => {
    if (e.key === 'Enter') {
        const expr = input.value;
        input.value = '';
        await evalExpr(expr);
    }
});
document.getElementById('terminal').addEventListener('click', () => input.focus());
//# sourceMappingURL=app.js.map