// src/parser.rs
use sexp::*;
use sexp::Atom::*;
use crate::ast::*;

pub fn is_keyword(s: &str) -> bool {
    matches!(s, 
        "let" | "add1" | "sub1" | "isnum" | "isbool" | 
        "+" | "-" | "*" | "<" | ">" | ">=" | "<=" | "=" |
        "if" | "block" | "loop" | "break" | "set!" | 
        "true" | "false" | "input" | "define"
    )
}

pub fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            if vec.len() != 2 {
                panic!("Invalid binding");
            }
            let name = match &vec[0] {
                Sexp::Atom(S(s)) => {
                    if is_keyword(s) {
                        panic!("keyword");
                    }
                    s.clone()
                },
                _ => panic!("Invalid binding: expected identifier"),
            };
            let expr = parse_expr(&vec[1]);
            (name, expr)
        }
        _ => panic!("Invalid binding: expected list"),
    }
}

pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            let n_i32 = i32::try_from(*n).unwrap();
            Expr::Number(n_i32)
        }
        Sexp::Atom(F(_)) => {
            panic!("floats not supported yet :)")
        }
        Sexp::Atom(S(name)) => {
            // reserved words
            match name.as_str() {
                // "let" | "add1" | "sub1" | "+" | "-" | "*" | "define" => {
                //     panic!("used reserved word {}", name)
                // }
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                "input" => Expr::Input,
                keyword if is_keyword(keyword) => {
                    panic!("keyword");
                }
                _ => Expr::Id(name.to_string()),
            }
        }
        Sexp::List(vec) => {
            if vec.is_empty() {
                panic!("empty expr");
            }
            
            match &vec[0] {
                Sexp::Atom(S(op)) => match op.as_str() {
                    "add1" | "sub1" => {
                        if vec.len() != 2 {
                            panic!("Invalid: {} takes exactly one argument", op);
                        }
                        let op_enum = match op.as_str() {
                            "add1" => Op1::Add1,
                            "sub1" => Op1::Sub1,
                            _ => unreachable!(),
                        };
                        Expr::UnOp(op_enum, Box::new(parse_expr(&vec[1])))
                    }
                    "isnum" => {
                        if vec.len() != 2 {
                            panic!("Invalid: isnum takes exactly one argument");
                        }
                        Expr::UnOp(Op1::IsNum, Box::new(parse_expr(&vec[1])))
                    }
                    "isbool" => {
                        if vec.len() != 2 {
                            panic!("Invalid: isbool takes exactly one argument");
                        }
                        Expr::UnOp(Op1::IsBool, Box::new(parse_expr(&vec[1])))
                    }
                    "+" | "-" | "*" => {
                        if vec.len() != 3 {
                            panic!("{} takes exactly two arguments", op);
                        }
                        let op_enum = match op.as_str() {
                            "+" => Op2::Plus,
                            "-" => Op2::Minus,
                            "*" => Op2::Times,
                            _ => unreachable!(),
                        };
                        Expr::BinOp(
                            op_enum,
                            Box::new(parse_expr(&vec[1])),
                            Box::new(parse_expr(&vec[2])),
                        )
                    }
                    "<" | ">" | ">=" | "<=" | "=" => {
                        if vec.len() != 3 {
                            panic!("Invalid: {} takes exactly two arguments", op);
                        }
                        let op_enum = match op.as_str() {
                            "<" => Op2::Less,
                            ">" => Op2::Greater,
                            ">=" => Op2::GreaterEqual,
                            "<=" => Op2::LessEqual,
                            "=" => Op2::Equal,
                            _ => unreachable!(),
                        };
                        Expr::BinOp(
                            op_enum,
                            Box::new(parse_expr(&vec[1])),
                            Box::new(parse_expr(&vec[2])),
                        )
                    }
                    "let" => {
                        if vec.len() != 3 {
                            panic!("let takes exactly two arguments");
                        }
                        let bindings_list = match &vec[1] {
                            Sexp::List(list) => list,
                            _ => panic!("let bindings must be a list"),
                        };
                        let mut bindings = Vec::new();
                        for binding_sexp in bindings_list {
                            bindings.push(parse_bind(binding_sexp));
                        }
                        if bindings.is_empty() {
                            panic!("let requires at least one binding");
                        }
                        Expr::Let(bindings, Box::new(parse_expr(&vec[2])))
                    }                   
                    "if" => { 
                            if vec.len() != 4 {
                                panic!("Invalid: if takes exactly three arguments");
                            }
                            Expr::If(
                                Box::new(parse_expr(&vec[1])),
                                Box::new(parse_expr(&vec[2])),
                                Box::new(parse_expr(&vec[3])),
                            )
                    }
                    "block" => {
                        if vec.len() < 2 {
                            panic!("Invalid: block requires at least one expression");
                        }
                        let mut exprs = Vec::new();
                        for expr_sexp in &vec[1..] {
                            exprs.push(parse_expr(expr_sexp));
                        }
                        Expr::Block(exprs)
                    }
                    "set!" => {
                        if vec.len() != 3 {
                            panic!("Invalid: set! requires exactly two arguments");
                        }
                        let name = match &vec[1] {
                            Sexp::Atom(S(s)) => {
                                if is_keyword(s) {
                                    panic!("keyword");
                                }
                                s.clone()
                            }
                            _ => panic!("Invalid: first argument to set! must be an identifier"),
                        };
                        let value_expr = parse_expr(&vec[2]);
                        Expr::Set(name, Box::new(value_expr))
                    }
                    "loop" => {
                        if vec.len() != 2 {
                            panic!("Invalid: loop requires exactly one argument");
                        }
                        Expr::Loop(Box::new(parse_expr(&vec[1])))
                    }
                    
                    "break" => {
                        if vec.len() != 2 {
                            panic!("Invalid: break requires exactly one argument");
                        }
                        Expr::Break(Box::new(parse_expr(&vec[1])))
                    }
                    _ => panic!("unknown operation {}", op),
                },
                _ => panic!("expected operation"),
            }
        }
    }
}

pub fn parse_repl_entry(s: &Sexp, depth: usize) -> Result<ReplEntry, String> {
    match s {
        Sexp::List(vec) if !vec.is_empty() => {
            if let Sexp::Atom(S(op)) = &vec[0] {
                if op == "define" {
                    if depth > 0 {
                        return Err("Invalid".to_string());
                    }
                    if vec.len() != 3 {
                        return Err("Invalid: define takes exactly two arguments".to_string());
                    }
                    let name = match &vec[1] {
                        Sexp::Atom(S(s)) => s.clone(),
                        _ => return Err("Invalid: define name must be identifier".to_string()),
                    };
                    let expr = parse_expr(&vec[2]);
                    return Ok(ReplEntry::Define(name, Box::new(expr)));
                }
            }
        }
        _ => {}
    }
    
    Ok(ReplEntry::Expr(parse_expr(s)))
}
