use crate::ast::AST;
use crate::derive::show_ast;

pub fn find_antiderivative(ast: &AST) -> AST {
    match ast {
        AST::Num(n) => AST::Mul(Box::new(AST::Num(*n)), Box::new(AST::Var)),
        AST::Add(a, b) => AST::Add(
            Box::new(find_antiderivative(a)),
            Box::new(find_antiderivative(b)),
        ),
        AST::Sub(a, b) => AST::Sub(
            Box::new(find_antiderivative(a)),
            Box::new(find_antiderivative(b)),
        ),
        AST::Mul(a, b) => {
            if let AST::Num(n) = a.as_ref() {
                if let AST::Cos(arg) = b.as_ref() {
                    if let AST::Mul(k, inner) = arg.as_ref() {
                        if let AST::Num(k_val) = k.as_ref() {
                            if *k_val != 0.0 && matches!(inner.as_ref(), AST::Var) {
                                return AST::Div(
                                    Box::new(AST::Mul(
                                        Box::new(AST::Num(*n)),
                                        Box::new(AST::Sin(Box::new(AST::Mul(
                                            Box::new(AST::Num(*k_val)),
                                            Box::new(AST::Var),
                                        )))),
                                    )),
                                    Box::new(AST::Num(*k_val)),
                                );
                            }
                        }
                    }
                }
                if let AST::Sin(arg) = b.as_ref() {
                    if let AST::Mul(k, inner) = arg.as_ref() {
                        if let AST::Num(k_val) = k.as_ref() {
                            if *k_val != 0.0 && matches!(inner.as_ref(), AST::Var) {
                                return AST::Div(
                                    Box::new(AST::Mul(
                                        Box::new(AST::Num(*n)),
                                        Box::new(AST::Sub(
                                            Box::new(AST::Num(0.0)),
                                            Box::new(AST::Cos(Box::new(AST::Mul(
                                                Box::new(AST::Num(*k_val)),
                                                Box::new(AST::Var),
                                            )))),
                                        )),
                                    )),
                                    Box::new(AST::Num(*k_val)),
                                );
                            }
                        }
                    }
                }
                if let AST::Exp(arg) = b.as_ref() {
                    if let AST::Mul(k, inner) = arg.as_ref() {
                        if let AST::Num(k_val) = k.as_ref() {
                            if *k_val != 0.0 && matches!(inner.as_ref(), AST::Var) {
                                return AST::Div(
                                    Box::new(AST::Mul(
                                        Box::new(AST::Num(*n)),
                                        Box::new(AST::Exp(Box::new(AST::Mul(
                                            Box::new(AST::Num(*k_val)),
                                            Box::new(AST::Var),
                                        )))),
                                    )),
                                    Box::new(AST::Num(*k_val)),
                                );
                            }
                        }
                    }
                }
            }
            AST::Mul(
                Box::new(find_antiderivative(a)),
                Box::new(find_antiderivative(b)),
            )
        }
        AST::Pow(a, b) => {
            if let AST::Var = a.as_ref() {
                if let AST::Num(n) = b.as_ref() {
                    if *n != -1.0 {
                        return AST::Div(
                            Box::new(AST::Pow(
                                Box::new(AST::Var),
                                Box::new(AST::Num(n + 1.0)),
                            )),
                            Box::new(AST::Num(n + 1.0)),
                        );
                    }
                }
            }
            AST::Mul(
                Box::new(AST::Num(1.0)),
                Box::new(AST::Log(Box::new(AST::Abs(Box::new(AST::Var))))),
            )
        }
        AST::Div(a, b) => {
            if let AST::Var = b.as_ref() {
                if let AST::Num(n) = a.as_ref() {
                    return AST::Mul(
                        Box::new(AST::Num(*n)),
                        Box::new(AST::Log(Box::new(AST::Abs(Box::new(AST::Var))))),
                    );
                }
            }
            find_antiderivative(&AST::Mul(
                a.clone(),
                Box::new(AST::Pow(b.clone(), Box::new(AST::Num(-1.0)))),
            ))
        }
        AST::Sin(a) => AST::Sub(
            Box::new(AST::Num(0.0)),
            Box::new(AST::Cos(a.clone())),
        ),
        AST::Cos(a) => AST::Sin(a.clone()),
        AST::Exp(a) => AST::Exp(a.clone()),
        _ => AST::Mul(Box::new(AST::Num(0.0)), Box::new(AST::Var)),
    }
}

pub fn find_antiderivative_string(ast: &AST) -> Option<String> {
    let result = find_antiderivative(ast);
    match &result {
        AST::Mul(c, _) => {
            if let AST::Num(n) = c.as_ref() {
                if *n == 0.0 { return None; }
            }
            Some(show_ast(&result))
        }
        _ => Some(show_ast(&result)),
    }
}
