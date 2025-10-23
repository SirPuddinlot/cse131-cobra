// src/ast.rs
#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal, 
    Greater, 
    GreaterEqual, 
    Less,
    LessEqual,
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Id(String),
    Input,
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Boolean(bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>),  // NEW
    Block(Vec<Expr>),  // NEW for later
    Set(String, Box<Expr>),  // NEW for later
    Loop(Box<Expr>),    
    Break(Box<Expr>),   
}

#[derive(Debug)]
pub enum ReplEntry {
    Expr(Expr),
    Define(String, Box<Expr>),
}
