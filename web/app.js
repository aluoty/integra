"use strict";
// ─── Complex numbers ─────────────────────────────────────
class C {
    constructor(re, im = 0) {
        this.re = re;
        this.im = im;
    }
    add(b) { return new C(this.re + b.re, this.im + b.im); }
    sub(b) { return new C(this.re - b.re, this.im - b.im); }
    mul(b) { return new C(this.re * b.re - this.im * b.im, this.re * b.im + this.im * b.re); }
    div(b) {
        const d = b.re * b.re + b.im * b.im;
        return new C((this.re * b.re + this.im * b.im) / d, (this.im * b.re - this.re * b.im) / d);
    }
    abs() { return Math.sqrt(this.re * this.re + this.im * this.im); }
    arg() { return Math.atan2(this.im, this.re); }
    conj() { return new C(this.re, -this.im); }
    ln() { return new C(Math.log(this.abs()), this.arg()); }
    pow(b) {
        if (this.re === 0 && this.im === 0)
            return new C(0);
        const r = this.abs(), theta = this.arg();
        const lnr = Math.log(r), newR = Math.exp(lnr * b.re - theta * b.im);
        const newTheta = theta * b.re + lnr * b.im;
        return new C(newR * Math.cos(newTheta), newR * Math.sin(newTheta));
    }
    eq(b) { return Math.abs(this.re - b.re) < 1e-12 && Math.abs(this.im - b.im) < 1e-12; }
    show() {
        if (Math.abs(this.im) < 1e-12)
            return niceNum(this.re);
        if (Math.abs(this.re) < 1e-12)
            return niceNum(this.im) + 'i';
        const s = this.im < 0 ? ' - ' : ' + ';
        return niceNum(this.re) + s + niceNum(Math.abs(this.im)) + 'i';
    }
    static from(n) { return new C(n, 0); }
}
function niceNum(n) {
    if (!isFinite(n))
        return n > 0 ? '∞' : '-∞';
    if (Math.abs(n) < 1e-12)
        return '0';
    if (Number.isInteger(n) && Math.abs(n) < 1e15)
        return n.toString();
    return parseFloat(n.toFixed(10)).toString().replace(/\.?0+$/, '');
}
const FUNCS = new Set(['sin', 'cos', 'tan', 'ln', 'log', 'log2', 'log10', 'exp', 'sqrt', 'abs', 'floor', 'ceil', 'round', 're', 'im', 'conj', 'sign', 'gamma', 'erf', 'sinh', 'cosh', 'tanh', 'asin', 'acos', 'atan', 'csc', 'sec', 'cot', 'cbrt', 'log1p', 'expm1']);
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
            else if (id === 'i')
                t.push({ t: 'num', v: 0 }); // handled as complex later
            else
                t.push({ t: 'id', v: id });
            continue;
        }
        return [];
    }
    return t;
}
function parse(s) {
    const toks = tokenize(s);
    if (toks.length === 0)
        return null;
    try {
        return parseExpr(toks, 0)[0];
    }
    catch {
        return null;
    }
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
    if (i < toks.length && toks[i].t === 'op' && toks[i].v === '+') {
        return parseUnary(toks, i + 1);
    }
    return parseAtom(toks, i);
}
function parseAtom(toks, i) {
    if (i >= toks.length)
        return [{ n: 'num', v: 0 }, i];
    const tk = toks[i];
    if (tk.t === 'num') {
        return [{ n: 'num', v: tk.v }, i + 1];
    }
    if (tk.t === 'id') {
        if (FUNCS.has(tk.v)) {
            if (i + 1 < toks.length && toks[i + 1].t === 'lp') {
                const [a, i2] = parseParen(toks, i + 1);
                return [{ n: 'call', fn: tk.v, a }, i2];
            }
            const [a, i2] = parseAtom(toks, i + 1);
            return [{ n: 'call', fn: tk.v, a }, i2];
        }
        if (tk.v === 'x')
            return [{ n: 'var' }, i + 1];
        return [{ n: 'num', v: 0 }, i + 1];
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
// ─── Eval (complex) ──────────────────────────────────────
function evalC(ast, x, ans) {
    switch (ast.n) {
        case 'num': return new C(ast.v);
        case 'var': return x;
        case 'un': {
            const v = evalC(ast.a, x, ans);
            return ast.op === '-' ? v.mul(new C(-1)) : v;
        }
        case 'bin': {
            const a = evalC(ast.a, x, ans), b = evalC(ast.b, x, ans);
            switch (ast.op) {
                case '+': return a.add(b);
                case '-': return a.sub(b);
                case '*': return a.mul(b);
                case '/': return a.div(b);
                case '^': return a.pow(b);
                default: return new C(0);
            }
        }
        case 'call': {
            const a = evalC(ast.a, x, ans);
            switch (ast.fn) {
                case 'sin': return new C(Math.sin(a.re) * Math.cosh(a.im), Math.cos(a.re) * Math.sinh(a.im));
                case 'cos': return new C(Math.cos(a.re) * Math.cosh(a.im), -Math.sin(a.re) * Math.sinh(a.im));
                case 'tan': {
                    const s = evalC({ n: 'call', fn: 'sin', a: ast.a }, x, ans), c = evalC({ n: 'call', fn: 'cos', a: ast.a }, x, ans);
                    return s.div(c);
                }
                case 'ln': return new C(Math.log(a.abs()), a.arg());
                case 'log': return evalC({ n: 'call', fn: 'ln', a: ast.a }, x, ans);
                case 'log2': return evalC({ n: 'call', fn: 'ln', a: ast.a }, x, ans).div(new C(Math.LN2));
                case 'log10': return evalC({ n: 'call', fn: 'ln', a: ast.a }, x, ans).div(new C(Math.LN10));
                case 'exp': return new C(Math.exp(a.re) * Math.cos(a.im), Math.exp(a.re) * Math.sin(a.im));
                case 'sqrt': return a.pow(new C(0.5));
                case 'cbrt': return a.pow(new C(1 / 3));
                case 'abs': return new C(a.abs());
                case 're': return new C(a.re);
                case 'im': return new C(a.im);
                case 'conj': return a.conj();
                case 'sign': return new C(a.re > 0 ? 1 : a.re < 0 ? -1 : 0);
                case 'floor': return new C(Math.floor(a.re));
                case 'ceil': return new C(Math.ceil(a.re));
                case 'round': return new C(Math.round(a.re));
                case 'sinh': return new C(Math.sinh(a.re) * Math.cos(a.im), Math.cosh(a.re) * Math.sin(a.im));
                case 'cosh': return new C(Math.cosh(a.re) * Math.cos(a.im), Math.sinh(a.re) * Math.sin(a.im));
                case 'tanh': {
                    const s = evalC({ n: 'call', fn: 'sinh', a: ast.a }, x, ans), c = evalC({ n: 'call', fn: 'cosh', a: ast.a }, x, ans);
                    return s.div(c);
                }
                case 'asin': return a.mul(new C(0, -1)).mul(new C(1).sub(a.mul(a)).pow(new C(0.5)).add(a.mul(new C(0, 1))).ln());
                case 'acos': return new C(Math.PI / 2).sub(evalC({ n: 'call', fn: 'asin', a: ast.a }, x, ans));
                case 'atan': return new C(0, 0.5).mul(new C(1).sub(a.mul(new C(0, 1))).div(new C(1).add(a.mul(new C(0, 1)))).ln());
                case 'csc': return evalC({ n: 'call', fn: 'sin', a: ast.a }, x, ans).pow(new C(-1));
                case 'sec': return evalC({ n: 'call', fn: 'cos', a: ast.a }, x, ans).pow(new C(-1));
                case 'cot': return evalC({ n: 'call', fn: 'tan', a: ast.a }, x, ans).pow(new C(-1));
                case 'log1p': return evalC({ n: 'call', fn: 'ln', a: { n: 'bin', op: '+', a: { n: 'num', v: 1 }, b: ast.a } }, x, ans);
                case 'expm1': return evalC({ n: 'bin', op: '-', a: { n: 'call', fn: 'exp', a: ast.a }, b: { n: 'num', v: 1 } }, x, ans);
                default: return new C(0);
            }
        }
    }
}
function evalReal(ast, x) { return evalC(ast, new C(x), new C(0)).re; }
// ─── Coefficient recovery (for solvers) ──────────────────
function coefLinear(ast) {
    const f0 = evalReal(ast, 0), f1 = evalReal(ast, 1);
    const a = f1 - f0, b = f0;
    return [a, b];
}
function coefQuadratic(ast) {
    const f0 = evalReal(ast, 0), f1 = evalReal(ast, 1), fm1 = evalReal(ast, -1);
    const a = (f1 + fm1) / 2 - f0;
    const b = (f1 - fm1) / 2;
    const c = f0;
    return [a, b, c];
}
function coefCubic(ast) {
    const f0 = evalReal(ast, 0), f1 = evalReal(ast, 1), fm1 = evalReal(ast, -1), f2 = evalReal(ast, 2);
    const a = (f2 - 2 * f1 + 2 * fm1 - f0) / 6;
    const b = (f1 + fm1 - 2 * f0 - 6 * a) / 2;
    const c = (f1 - fm1) / 2;
    const d = f0;
    return [a, b, c, d];
}
// ─── Solvers ──────────────────────────────────────────────
function solveLinear(ast) {
    const [a, b] = coefLinear(ast);
    if (Math.abs(a) < 1e-12)
        return Math.abs(b) < 1e-12 ? 'All real numbers' : 'No solution';
    return 'x = ' + niceNum(-b / a);
}
function solveQuadratic(ast) {
    const [a, b, c] = coefQuadratic(ast);
    if (Math.abs(a) < 1e-12)
        return solveLinear(ast);
    const disc = b * b - 4 * a * c;
    if (disc < 0) {
        const real = -b / (2 * a), imag = Math.sqrt(-disc) / (2 * a);
        return 'x = ' + niceNum(real) + ' ± ' + niceNum(imag) + 'i';
    }
    const sqrtD = Math.sqrt(disc);
    const x1 = (-b + sqrtD) / (2 * a), x2 = (-b - sqrtD) / (2 * a);
    return 'x = ' + niceNum(x1) + ', x = ' + niceNum(x2);
}
function solveCubic(ast) {
    const [a, b, c, d] = coefCubic(ast);
    if (Math.abs(a) < 1e-12)
        return solveQuadratic(ast);
    // Normalize: x^3 + px + q = 0
    const p = (3 * a * c - b * b) / (3 * a * a);
    const q = (2 * b * b * b - 9 * a * b * c + 27 * a * a * d) / (27 * a * a * a);
    const disc = q * q / 4 + p * p * p / 27;
    if (disc > 0) {
        // One real root
        const u = new C(-q / 2).add(new C(Math.sqrt(disc)));
        const v = new C(-q / 2).sub(new C(Math.sqrt(disc)));
        const cu = u.pow(new C(1 / 3)), cv = v.pow(new C(1 / 3));
        const x0 = cu.add(cv).re - b / (3 * a);
        return 'x = ' + niceNum(x0);
    }
    else if (Math.abs(disc) < 1e-12) {
        // Multiple roots
        const u = Math.cbrt(-q / 2);
        const x0 = 2 * u - b / (3 * a);
        const x1 = -u - b / (3 * a);
        return 'x = ' + niceNum(x0) + (Math.abs(x1 - x0) > 1e-10 ? ', x = ' + niceNum(x1) : '');
    }
    else {
        // Three real roots (casus irreducibilis)
        const r = Math.sqrt(-p * p * p / 27);
        const phi = Math.acos(-q / (2 * r));
        const sqrt3p = 2 * Math.sqrt(-p / 3);
        const x0 = sqrt3p * Math.cos(phi / 3) - b / (3 * a);
        const x1 = sqrt3p * Math.cos((phi + 2 * Math.PI) / 3) - b / (3 * a);
        const x2 = sqrt3p * Math.cos((phi + 4 * Math.PI) / 3) - b / (3 * a);
        return 'x = ' + niceNum(x0) + ', x = ' + niceNum(x1) + ', x = ' + niceNum(x2);
    }
}
// ─── Numerical methods ───────────────────────────────────
function adaptSimpson(f, a, b) {
    function simpson(lo, hi) { const m = (lo + hi) / 2; return (hi - lo) / 6 * (f(lo) + 4 * f(m) + f(hi)); }
    function go(lo, hi, whole, tol) {
        const m = (lo + hi) / 2, left = simpson(lo, m), right = simpson(m, hi);
        if (Math.abs(left + right - whole) < 15 * tol)
            return left + right + (left + right - whole) / 15;
        return go(lo, m, left, tol / 2) + go(m, hi, right, tol / 2);
    }
    return go(a, b, simpson(a, b), 1e-8);
}
function derivative(f, x) { const h = 1e-8; return (f(x + h) - f(x - h)) / (2 * h); }
function derivative2(f, x) { const h = 1e-5; return (f(x - h) - 2 * f(x) + f(x + h)) / (h * h); }
function derivativeN(n, f, x) {
    if (n === 0)
        return f(x);
    if (n === 1)
        return derivative(f, x);
    if (n === 2)
        return derivative2(f, x);
    const h = Math.max(1e-8, 1e-3 / n);
    return (derivativeN(n - 1, f, x + h) - derivativeN(n - 1, f, x - h)) / (2 * h);
}
function limit(f, a) { return f(a + 1e-10); }
function taylorSeries(f, a, n, atX) {
    let sum = 0, fact = 1;
    for (let i = 0; i <= n; i++) {
        const term = derivativeN(i, f, a) / fact * Math.pow(atX - a, i);
        sum += term;
        fact *= (i + 1);
    }
    return sum;
}
// ─── SVG generation ──────────────────────────────────────
function generateGraphSVG(ast, xMin, xMax, yMin, yMax) {
    const w = 600, h = 400, steps = 400;
    const pts = [];
    for (let i = 0; i <= steps; i++) {
        const x = xMin + (xMax - xMin) * i / steps;
        const y = evalReal(ast, x);
        if (isFinite(y))
            pts.push([x, y]);
    }
    if (pts.length === 0)
        return '<svg width="600" height="400"><text x="10" y="20">No points</text></svg>';
    const ys = pts.map(p => p[1]);
    const yLo = yMin ?? Math.min(...ys) - (Math.max(...ys) - Math.min(...ys)) * 0.1;
    const yHi = yMax ?? Math.max(...ys) + (Math.max(...ys) - Math.min(...ys)) * 0.1;
    const mx = (x) => (x - xMin) / (xMax - xMin) * w;
    const my = (y) => h - (y - yLo) / (yHi - yLo) * h;
    const path = pts.map(([x, y], i) => (i === 0 ? 'M' : 'L') + mx(x).toFixed(2) + ',' + my(y).toFixed(2)).join(' ');
    const grid = (n, axis) => {
        let s = '';
        const lo = axis === 'x' ? xMin : yLo, hi = axis === 'x' ? xMax : yHi;
        for (let i = 0; i <= n; i++) {
            const v = lo + (hi - lo) * i / n;
            const px = axis === 'x' ? mx(v) : (i / n * w), py = axis === 'x' ? (i / n * h) : my(v);
            s += '<line x1="' + (axis === 'x' ? px : 0) + '" y1="' + (axis === 'x' ? 0 : py) + '" x2="' + (axis === 'x' ? px : w) + '" y2="' + (axis === 'x' ? h : py) + '" stroke="#ddd" stroke-width="0.5"/>';
            s += '<text x="' + (axis === 'x' ? px : 5) + '" y="' + (axis === 'x' ? h - 5 : py - 3) + '" text-anchor="middle" font-size="10" fill="#666">' + niceNum(v) + '</text>';
        }
        return s;
    };
    const x0 = mx(0), y0 = my(0);
    return '<svg xmlns="http://www.w3.org/2000/svg" width="' + w + '" height="' + h + '">'
        + '<rect width="' + w + '" height="' + h + '" fill="white"/>'
        + grid(10, 'x') + grid(10, 'y')
        + '<line x1="' + x0 + '" y1="0" x2="' + x0 + '" y2="' + h + '" stroke="#999" stroke-width="1"/>'
        + '<line x1="0" y1="' + y0 + '" x2="' + w + '" y2="' + y0 + '" stroke="#999" stroke-width="1"/>'
        + '<path d="' + path + '" fill="none" stroke="#2563eb" stroke-width="2"/>'
        + '</svg>';
}
function generateIntegralSVG(ast, a, b) {
    const pad = (b - a) * 0.1;
    return generateGraphSVG(ast, a - pad, b + pad, undefined, undefined);
}
function generateFractalSVG(kind, w, h, maxIter, xMin, xMax, yMin, yMax, cx, cy) {
    const xStep = (xMax - xMin) / w, yStep = (yMax - yMin) / h;
    const c = kind === 'julia' && cx !== undefined && cy !== undefined ? new C(cx, cy) : new C(0);
    function iter(px, py) {
        let z = kind === 'julia' ? new C(px, py) : new C(0);
        const param = kind === 'julia' ? c : new C(px, py);
        const x0 = xMin + px * xStep, y0 = yMin + py * yStep;
        if (kind === 'julia') {
            z = new C(x0, y0);
        }
        else {
            z = new C(0);
        }
        const cParam = kind === 'julia' ? c : new C(x0, y0);
        for (let n = 0; n < maxIter; n++) {
            if (z.abs() > 2) {
                return n + 1 - Math.log(Math.log(z.abs())) / Math.LN2;
            }
            if (kind === 'burningship') {
                z = new C(z.re * z.re - z.im * z.im + cParam.re, 2 * Math.abs(z.re * z.im) + cParam.im);
            }
            else {
                z = z.mul(z).add(cParam);
            }
        }
        return 0;
    }
    function smoothColor(mu) {
        if (mu === 0)
            return '#000';
        const t = mu / maxIter;
        const hue = 360 * 4 * t;
        const light = 45 + 40 * Math.sin(Math.PI * t);
        return 'hsl(' + hue + ',100%,' + light + '%)';
    }
    const rects = [];
    for (let px = 0; px < w; px++) {
        for (let py = 0; py < h; py++) {
            const mu = iter(px, py);
            rects.push('<rect x="' + px + '" y="' + py + '" width="1" height="1" fill="' + smoothColor(mu) + '"/>');
        }
    }
    return '<svg xmlns="http://www.w3.org/2000/svg" width="' + w + '" height="' + h + '">' + rects.join('') + '</svg>';
}
// ─── Symbolic derivative ─────────────────────────────────
function derivAST(ast) {
    switch (ast.n) {
        case 'num': return { n: 'num', v: 0 };
        case 'var': return { n: 'num', v: 1 };
        case 'un': return { n: 'un', op: '-', a: derivAST(ast.a) };
        case 'bin': {
            const a = ast.a, b = ast.b;
            switch (ast.op) {
                case '+': return { n: 'bin', op: '+', a: derivAST(a), b: derivAST(b) };
                case '-': return { n: 'bin', op: '-', a: derivAST(a), b: derivAST(b) };
                case '*': return { n: 'bin', op: '+', a: { n: 'bin', op: '*', a: derivAST(a), b }, b: { n: 'bin', op: '*', a, b: derivAST(b) } };
                case '/': return { n: 'bin', op: '/', a: { n: 'bin', op: '-', a: { n: 'bin', op: '*', a: derivAST(a), b }, b: { n: 'bin', op: '*', a, b: derivAST(b) } }, b: { n: 'bin', op: '^', a: b, b: { n: 'num', v: 2 } } };
                case '^':
                    if (b.n === 'num')
                        return { n: 'bin', op: '*', a: { n: 'bin', op: '*', a: { n: 'num', v: b.v }, b: { n: 'bin', op: '^', a, b: { n: 'num', v: b.v - 1 } } }, b: derivAST(a) };
                    return { n: 'bin', op: '*', a: { n: 'bin', op: '^', a, b }, b: derivAST({ n: 'bin', op: '*', a: { n: 'call', fn: 'ln', a }, b }) };
                default: return { n: 'num', v: 0 };
            }
        }
        case 'call': {
            const a = ast.a;
            switch (ast.fn) {
                case 'sin': return { n: 'bin', op: '*', a: { n: 'call', fn: 'cos', a }, b: derivAST(a) };
                case 'cos': return { n: 'bin', op: '*', a: { n: 'un', op: '-', a: { n: 'call', fn: 'sin', a } }, b: derivAST(a) };
                case 'tan': return { n: 'bin', op: '*', a: { n: 'bin', op: '+', a: { n: 'num', v: 1 }, b: { n: 'bin', op: '^', a: { n: 'call', fn: 'tan', a }, b: { n: 'num', v: 2 } } }, b: derivAST(a) };
                case 'ln': return { n: 'bin', op: '/', a: derivAST(a), b: a };
                case 'exp': return { n: 'bin', op: '*', a: { n: 'call', fn: 'exp', a }, b: derivAST(a) };
                case 'log': return derivAST({ n: 'bin', op: '/', a: { n: 'call', fn: 'ln', a }, b: { n: 'call', fn: 'ln', a: { n: 'num', v: 10 } } });
                case 'sqrt': return derivAST({ n: 'bin', op: '^', a, b: { n: 'num', v: 0.5 } });
                case 'cbrt': return derivAST({ n: 'bin', op: '^', a, b: { n: 'num', v: 1 / 3 } });
                default: return { n: 'num', v: 0 };
            }
        }
    }
}
// ─── Show AST as string ──────────────────────────────────
function showAST(ast, prec = 0) {
    const needParen = (p) => prec > p;
    switch (ast.n) {
        case 'num': return niceNum(ast.v);
        case 'var': return 'x';
        case 'un': return '-' + showAST(ast.a, 5);
        case 'bin': {
            const p = ast.op === '+' || ast.op === '-' ? 1 : ast.op === '*' || ast.op === '/' ? 2 : ast.op === '^' ? 4 : 0;
            const a = showAST(ast.a, p), b = showAST(ast.b, ast.op === '^' ? 4 : p + 1);
            return (needParen(p) ? '(' : '') + a + ' ' + ast.op + ' ' + b + (needParen(p) ? ')' : '');
        }
        case 'call': return ast.fn + '(' + showAST(ast.a, 0) + ')';
    }
}
// ─── Antiderivative patterns ─────────────────────────────
function antiderivAST(ast) {
    if (ast.n === 'num')
        return { n: 'bin', op: '*', a: ast, b: { n: 'var' } };
    if (ast.n === 'var')
        return { n: 'bin', op: '/', a: { n: 'bin', op: '^', a: { n: 'var' }, b: { n: 'num', v: 2 } }, b: { n: 'num', v: 2 } };
    if (ast.n === 'bin' && ast.op === '+') {
        const l = antiderivAST(ast.a), r = antiderivAST(ast.b);
        if (l && r)
            return { n: 'bin', op: '+', a: l, b: r };
    }
    if (ast.n === 'bin' && ast.op === '-') {
        const l = antiderivAST(ast.a), r = antiderivAST(ast.b);
        if (l && r)
            return { n: 'bin', op: '-', a: l, b: r };
    }
    if (ast.n === 'call') {
        if (ast.fn === 'sin')
            return { n: 'un', op: '-', a: { n: 'call', fn: 'cos', a: ast.a } };
        if (ast.fn === 'cos')
            return { n: 'call', fn: 'sin', a: ast.a };
        if (ast.fn === 'exp')
            return { n: 'call', fn: 'exp', a: ast.a };
        if (ast.fn === 'ln')
            return { n: 'bin', op: '-', a: { n: 'bin', op: '*', a: { n: 'var' }, b: ast }, b: { n: 'var' } };
    }
    return null;
}
const API_BASE = (typeof INTEGRA_API_BASE !== 'undefined' ? INTEGRA_API_BASE : '') + '/api';
const input = document.getElementById('input');
const output = document.getElementById('output');
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
        throw new Error('HTTP ' + res.status);
    return res.json();
}
async function fetchText(url) {
    const res = await fetch(url);
    if (!res.ok)
        throw new Error('HTTP ' + res.status);
    return res.text();
}
// ─── Command dispatch ────────────────────────────────────
async function evalExpr(expr) {
    addLine('λ ' + expr, 'input');
    if (!expr.trim())
        return;
    if (expr.startsWith(':')) {
        await handleCommand(expr);
        return;
    }
    const ast = parse(expr);
    if (ast) {
        try {
            const result = evalC(ast, new C(0), new C(0));
            if (isFinite(result.re) && isFinite(result.im)) {
                addLine(result.show(), 'result');
                return;
            }
        }
        catch { }
    }
    addLine('Error: invalid expression', 'error');
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
            addLine('  :quit                     Close tab', 'info');
            addLine('  :deriv <expr> [at <x>]    Derivative', 'info');
            addLine('  :deriv2 <expr> at <x>     Numerical 2nd derivative', 'info');
            addLine('  :derivn <expr> order <n> at <x>', 'info');
            addLine('  :integral <expr> [from <a> to <b>]', 'info');
            addLine('  :limit <expr> as <x>      Limit', 'info');
            addLine('  :taylor <expr> at <a> order <n>', 'info');
            addLine('  :solve <expr>             Solve linear', 'info');
            addLine('  :solveq <expr>            Solve quadratic', 'info');
            addLine('  :solvec <expr>            Solve cubic', 'info');
            addLine('  :graph <expr> [from <a> to <b>] [yfrom <ya> to <yb>]', 'info');
            addLine('  :mandelbrot [w h iter xmin xmax ymin ymax]', 'info');
            addLine('  :julia <cx> <cy> [w h iter xmin xmax ymin ymax]', 'info');
            addLine('  :burningship [w h iter xmin xmax ymin ymax]', 'info');
            addLine('  :explain deriv|integral|solve <expr>', 'info');
            break;
        case 'about':
            addLine('Integra v1.0 — Web Calculator\nComplex numbers, calculus, graphing, fractals\nBuilt with TypeScript', 'info');
            break;
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
            const ast = parse(exprStr);
            if (!ast) {
                addLine('Error: cannot parse expression', 'error');
                break;
            }
            const d = derivAST(ast);
            addLine("f'(x) = " + showAST(d), 'deriv');
            if (atStr) {
                const atAst = parse(atStr);
                if (atAst) {
                    const xVal = evalC(atAst, new C(0), new C(0));
                    const result = evalC(d, xVal, new C(0));
                    addLine("f'(" + atStr + ') = ' + result.show(), 'result');
                }
                else {
                    addLine("f'(" + atStr + ') = ' + evalC(d, new C(parseFloat(atStr) || 0), new C(0)).show(), 'result');
                }
            }
            break;
        }
        case 'deriv2': {
            const m = rest.match(/^(.*?)\s+at\s+(.+)$/);
            if (!m) {
                addLine('Usage: :deriv2 <expr> at <x>', 'error');
                break;
            }
            const ast = parse(m[1]);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            const x = parseFloat(m[2]);
            if (!isFinite(x)) {
                addLine('Error: invalid x', 'error');
                break;
            }
            const r = derivative2((xv) => evalReal(ast, xv), x);
            addLine("f''(" + m[2] + ') = ' + niceNum(r), 'result');
            break;
        }
        case 'derivn': {
            const m = rest.match(/^(.*?)\s+order\s+(.+?)\s+at\s+(.+)$/);
            if (!m) {
                addLine('Usage: :derivn <expr> order <n> at <x>', 'error');
                break;
            }
            const ast = parse(m[1]);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            const n = parseInt(m[2]), x = parseFloat(m[3]);
            if (!isFinite(n) || !isFinite(x)) {
                addLine('Error: invalid n or x', 'error');
                break;
            }
            const r = derivativeN(n, (xv) => evalReal(ast, xv), x);
            addLine("f^(" + m[2] + ")(" + m[3] + ') = ' + niceNum(r), 'result');
            break;
        }
        case 'integral': {
            const m = rest.match(/^(.*?)\s+from\s+(.+?)\s+to\s+(.+)$/);
            if (m) {
                const ast = parse(m[1]);
                if (!ast) {
                    addLine('Error: cannot parse', 'error');
                    break;
                }
                const a = parseFloat(m[2]), b = parseFloat(m[3]);
                if (!isFinite(a) || !isFinite(b)) {
                    addLine('Error: invalid bounds', 'error');
                    break;
                }
                const r = adaptSimpson((xv) => evalReal(ast, xv), a, b);
                addLine('∫ f(x) dx = ' + niceNum(r), 'result');
                addHTML('<div class="svg-container">' + generateIntegralSVG(ast, a, b) + '</div>');
            }
            else {
                const ast = parse(rest);
                if (!ast) {
                    addLine('Error: cannot parse', 'error');
                    break;
                }
                const ad = antiderivAST(ast);
                addLine('∫ f(x) dx = ' + (ad ? showAST(ad) + ' + C' : '? (cannot find antiderivative)'), 'result');
            }
            break;
        }
        case 'limit': {
            const m = rest.match(/^(.*?)\s+as\s+(.+)$/);
            if (!m) {
                addLine('Usage: :limit <expr> as <x>', 'error');
                break;
            }
            const ast = parse(m[1]);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            const x = parseFloat(m[2]);
            if (!isFinite(x)) {
                addLine('Error: invalid x', 'error');
                break;
            }
            const r = limit((xv) => evalReal(ast, xv), x);
            addLine('lim f(x) as x → ' + m[2] + ' = ' + niceNum(r), 'result');
            break;
        }
        case 'taylor': {
            const m = rest.match(/^(.*?)\s+at\s+(.+?)\s+order\s+(.+)$/);
            if (!m) {
                addLine('Usage: :taylor <expr> at <a> order <n>', 'error');
                break;
            }
            const ast = parse(m[1]);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            const a = parseFloat(m[2]), n = parseInt(m[3]);
            if (!isFinite(a) || !isFinite(n)) {
                addLine('Error: invalid a or n', 'error');
                break;
            }
            const r = taylorSeries((xv) => evalReal(ast, xv), a, Math.min(n, 10), 0);
            addLine('T_' + m[3] + '(0) ≈ ' + niceNum(r), 'result');
            break;
        }
        case 'solve':
        case 'solveq':
        case 'solvec': {
            const eqParts = rest.split(/(?<!=)=(?!=)/); // split on single =
            const expr = eqParts.length === 2 ? '(' + eqParts[0] + ')-(' + eqParts[1] + ')' : rest;
            const ast = parse(expr);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            const solvers = { solve: solveLinear, solveq: solveQuadratic, solvec: solveCubic };
            addLine(solvers[command](ast), 'result');
            break;
        }
        case 'explain': {
            const m = rest.match(/^(deriv|integral|solve)\s+(.+)$/);
            if (!m) {
                addLine('Usage: :explain deriv|integral|solve <expr>', 'error');
                break;
            }
            const ast = parse(m[2]);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            if (m[1] === 'deriv') {
                const d = derivAST(ast);
                addLine('f(x) = ' + showAST(ast) + "\nf'(x) = " + showAST(d), 'info');
            }
            else if (m[1] === 'integral') {
                const ad = antiderivAST(ast);
                addLine('f(x) = ' + showAST(ast) + '\n∫ f(x) dx = ' + (ad ? showAST(ad) + ' + C' : '?'), 'info');
            }
            else {
                addLine('Equation: ' + showAST(ast) + ' = 0\nSolution: ' + solveLinear(ast), 'info');
            }
            break;
        }
        case 'graph': {
            const m = rest.match(/^(.*?)(?:\s+from\s+(.+?)\s+to\s+(.+?))?(?:\s+yfrom\s+(.+?)\s+to\s+(.+))?$/);
            const exprStr = m[1];
            const ast = parse(exprStr);
            if (!ast) {
                addLine('Error: cannot parse', 'error');
                break;
            }
            const xMin = m[2] ? parseFloat(m[2]) : -10;
            const xMax = m[3] ? parseFloat(m[3]) : 10;
            const yMin = m[4] ? parseFloat(m[4]) : undefined;
            const yMax = m[5] ? parseFloat(m[5]) : undefined;
            if (!isFinite(xMin) || !isFinite(xMax)) {
                addLine('Error: invalid bounds', 'error');
                break;
            }
            const svg = generateGraphSVG(ast, xMin, xMax, yMin, yMax);
            addHTML('<div class="svg-container">' + svg + '</div>');
            break;
        }
        case 'mandelbrot': {
            const args = rest.split(/\s+/);
            const w = parseInt(args[0]) || 100, h = parseInt(args[1]) || 100, iter = parseInt(args[2]) || 50;
            const xMin = args[3] ? parseFloat(args[3]) : -2.5, xMax = args[4] ? parseFloat(args[4]) : 1.0;
            const yMin = args[5] ? parseFloat(args[5]) : -1.25, yMax = args[6] ? parseFloat(args[6]) : 1.25;
            const svg = generateFractalSVG('mandelbrot', w, h, iter, xMin, xMax, yMin, yMax);
            addHTML('<div class="svg-container">' + svg + '</div>');
            break;
        }
        case 'julia': {
            const args = rest.split(/\s+/);
            const cx = parseFloat(args[0]) || -0.7, cy = parseFloat(args[1]) || 0.27015;
            const w = parseInt(args[2]) || 100, h = parseInt(args[3]) || 100, iter = parseInt(args[4]) || 50;
            const xMin = args[5] ? parseFloat(args[5]) : -2.0, xMax = args[6] ? parseFloat(args[6]) : 2.0;
            const yMin = args[7] ? parseFloat(args[7]) : -1.5, yMax = args[8] ? parseFloat(args[8]) : 1.5;
            const svg = generateFractalSVG('julia', w, h, iter, xMin, xMax, yMin, yMax, cx, cy);
            addHTML('<div class="svg-container">' + svg + '</div>');
            break;
        }
        case 'burningship': {
            const args = rest.split(/\s+/);
            const w = parseInt(args[0]) || 100, h = parseInt(args[1]) || 100, iter = parseInt(args[2]) || 50;
            const xMin = args[3] ? parseFloat(args[3]) : -2.5, xMax = args[4] ? parseFloat(args[4]) : 1.5;
            const yMin = args[5] ? parseFloat(args[5]) : -2.0, yMax = args[6] ? parseFloat(args[6]) : 1.0;
            const svg = generateFractalSVG('burningship', w, h, iter, xMin, xMax, yMin, yMax);
            addHTML('<div class="svg-container">' + svg + '</div>');
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