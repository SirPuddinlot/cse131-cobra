use std::env;
use std::fs::File;
use std::io::{self, prelude::*, BufRead};

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use dynasmrt::{dynasm, DynasmApi, x64::Assembler};
use std::mem;

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug, Clone, Copy)]
enum Reg {
    RAX,
    RSP,
    RBX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum ReplEntry {
    Expr(Expr),
    Define(String, Box<Expr>),
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => match reg {
            Reg::RAX => "rax".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::RBX => "rbx".to_string(),
        },
        Val::Imm(n) => format!("{}", n),
        Val::RegOffset(reg, offset) => {
            let reg_str = match reg {
                Reg::RAX => "rax",
                Reg::RSP => "rsp",
                Reg::RBX => "rbx",
            };
            format!("[{} - {}]", reg_str, -offset)
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dest, src) => format!("  mov {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IAdd(dest, src) => format!("  add {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::ISub(dest, src) => format!("  sub {}, {}", val_to_str(dest), val_to_str(src)),
        Instr::IMul(dest, src) => format!("  imul {}, {}", val_to_str(dest), val_to_str(src)),
    }
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, defines: &HashMap<String, i64>) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
        Expr::Id(name) => {
            // Check stack environment first to allow shadowing of defined variables
            if let Some(&offset) = env.get(name) {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))]
            }
            // Otherwise check if it's a defined variable (stored on heap)
            else if let Some(&val) = defines.get(name) {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(val as i32))]
            } 
            else {
                panic!("Unbound variable identifier {}", name);
            }
        }
        Expr::UnOp(op, expr) => {
            let mut code = compile_to_instrs(expr, si, env, defines);
            match op {
                Op1::Add1 => code.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1))),
                Op1::Sub1 => code.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1))),
            }
            code
        }
        Expr::BinOp(op, left, right) => {
            let mut code = compile_to_instrs(left, si, env, defines);
            
            // Save left result on stack
            code.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            
            // Compile right
            let right_code = compile_to_instrs(right, si - 8, env, defines);
            code.extend(right_code);
            
            // Perform operation
            match op {
                Op2::Plus => {
                    code.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                }
                Op2::Minus => {
                    code.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Times => {
                    code.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                }
            }
            code
        }
        Expr::Let(bindings, body) => {
            let mut code = Vec::new();
            let mut new_env = env.clone();
            let mut current_si = si;
            
            // Check for duplicate bindings
            let mut seen_names = HashMap::new();
            for (name, _) in bindings {
                if seen_names.contains_key(name) {
                    panic!("Duplicate binding");
                }
                seen_names = seen_names.update(name.clone(), ());
            }
            
            // Compile each binding
            for (name, expr) in bindings.iter() {
                let expr_code = compile_to_instrs(expr, current_si - 8, &new_env, defines);
                code.extend(expr_code);
                
                // Store result on stack
                code.push(Instr::IMov(Val::RegOffset(Reg::RSP, current_si), Val::Reg(Reg::RAX)));
                new_env = new_env.update(name.clone(), current_si);
                current_si -= 8;
            }
            
            // Compile body with new environment
            let body_code = compile_to_instrs(body, current_si, &new_env, defines);
            code.extend(body_code);
            
            code
        }
    }
}

fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e, -8, &HashMap::new(), &HashMap::new());
    let mut asm_code = String::new();
    
    for instr in instrs {
        asm_code.push_str(&instr_to_str(&instr));
        asm_code.push('\n');
    }
    
    asm_code
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => {
            if vec.len() != 2 {
                panic!("Invalid binding");
            }
            let name = match &vec[0] {
                Sexp::Atom(S(s)) => s.clone(),
                _ => panic!("Invalid binding: expected identifier"),
            };
            let expr = parse_expr(&vec[1]);
            (name, expr)
        }
        _ => panic!("Invalid binding: expected list"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
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
                "let" | "add1" | "sub1" | "+" | "-" | "*" | "define" => {
                    panic!("used reserved word {}", name)
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
                    "add1" => {
                        if vec.len() != 2 {
                            panic!("add1 takes exactly one argument");
                        }
                        Expr::UnOp(Op1::Add1, Box::new(parse_expr(&vec[1])))
                    }
                    "sub1" => {
                        if vec.len() != 2 {
                            panic!("sub1 takes exactly one argument");
                        }
                        Expr::UnOp(Op1::Sub1, Box::new(parse_expr(&vec[1])))
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
                    _ => panic!("unknown operation {}", op),
                },
                _ => panic!("expected operation"),
            }
        }
    }
}

fn parse_repl_entry(s: &Sexp, depth: usize) -> Result<ReplEntry, String> {
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

// JIT compilation - reuses compile_to_instrs!
fn compile_to_jit(e: &Expr, ops: &mut Assembler, defines: &HashMap<String, i64>) {
    let instrs = compile_to_instrs(e, -8, &HashMap::new(), defines);
    
    for instr in &instrs {
        instr_to_dynasm(instr, ops);
    }
}

fn instr_to_dynasm(instr: &Instr, ops: &mut Assembler) {
    match instr {
        Instr::IMov(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, QWORD *n as i64
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, rbx
                    );
                }
                (Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; mov rbx, rax
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; mov rax, [rsp - pos_offset]
                    );
                }
                (Val::RegOffset(Reg::RSP, offset), Val::Reg(Reg::RAX)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; mov [rsp - pos_offset], rax
                    );
                }
                _ => panic!("Unsupported mov pattern in JIT: {:?} <- {:?}", dest, src),
            }
        }
        Instr::IAdd(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; add rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; add rax, rbx
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; add rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported add pattern in JIT: {:?} += {:?}", dest, src),
            }
        }
        Instr::ISub(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; sub rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; sub rax, rbx
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; sub rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported sub pattern in JIT: {:?} -= {:?}", dest, src),
            }
        }
        Instr::IMul(dest, src) => {
            match (dest, src) {
                (Val::Reg(Reg::RAX), Val::Imm(n)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; imul rax, rax, *n as i32
                    );
                }
                (Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)) => {
                    dynasm!(ops
                        ; .arch x64
                        ; imul rax, rbx
                    );
                }
                (Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset)) => {
                    let pos_offset = -offset;
                    dynasm!(ops
                        ; .arch x64
                        ; imul rax, [rsp - pos_offset]
                    );
                }
                _ => panic!("Unsupported imul pattern in JIT: {:?} *= {:?}", dest, src),
            }
        }
    }
}

fn run_repl() -> io::Result<()> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let mut defines: HashMap<String, i64> = HashMap::new();
    
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    
    loop {
        print!("> ");
        io::stdout().flush()?;
        
        let mut input = String::new();
        reader.read_line(&mut input)?;
        
        let input = input.trim();
        
        // Check for exit commands
        if input == "exit" || input == "quit" {
            break;
        }
        
        if input.is_empty() {
            continue;
        }
        
        // Parse the input
        let sexp = match sexp::parse(input) {
            Ok(s) => s,
            Err(_e) => {
                println!("Invalid: parse error");
                continue;
            }
        };
        
        // Parse into ReplEntry
        let entry = match parse_repl_entry(&sexp, 0) {
            Ok(e) => e,
            Err(msg) => {
                println!("{}", msg);
                continue;
            }
        };
        
        match entry {
            ReplEntry::Define(name, expr) => {
                // Check for duplicate definition
                if defines.contains_key(&name) {
                    println!("Duplicate binding");
                    continue;
                }
                
                // Evaluate the expression
                let start = ops.offset();
                
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    compile_to_jit(&expr, &mut ops, &defines);
                })) {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                dynasm!(ops ; .arch x64 ; ret);
                
                match ops.commit() {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                let reader = ops.reader();
                let buf = reader.lock();
                let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
                let result = jitted_fn();
                
                // Store the result
                defines = defines.update(name, result);
                // Don't print anything for define
            }
            ReplEntry::Expr(expr) => {
                let start = ops.offset();
                
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    compile_to_jit(&expr, &mut ops, &defines);
                })) {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                dynasm!(ops ; .arch x64 ; ret);
                
                match ops.commit() {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                let reader = ops.reader();
                let buf = reader.lock();
                let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
                let result = jitted_fn();
                
                println!("{}", result);
            }
        }
    }
    
    Ok(())
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <-c|-e|-g|-i> <input.snek> [output.s]", args[0]);
        eprintln!("  -c: Compile to assembly file (requires output file)");
        eprintln!("  -e: Execute directly using JIT compilation");
        eprintln!("  -g: Do both - execute and generate assembly");
        eprintln!("  -i: Interactive REPL mode");
        std::process::exit(1);
    }

    let flag = &args[1];
    
    match flag.as_str() {
        "-i" => {
            // REPL mode
            return run_repl();
        }
        _ => {}
    }
    
    if args.len() < 3 {
        eprintln!("Usage: {} <-c|-e|-g> <input.snek> [output.s]", args[0]);
        eprintln!("  -c: Compile to assembly file (requires output file)");
        eprintln!("  -e: Execute directly using JIT compilation");
        eprintln!("  -g: Do both - execute and generate assembly");
        std::process::exit(1);
    }

    let in_name = &args[2];
    
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse the S-expression from the input
    let sexp = sexp::parse(&in_contents).map_err(|e| {
        std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Parse error: {}", e))
    })?;

    let expr = parse_expr(&sexp);
    
    match flag.as_str() {
        "-c" => {
            // AOT compilation only
            if args.len() < 4 {
                eprintln!("Error: -c flag requires output file");
                eprintln!("Usage: {} -c <input.snek> <output.s>", args[0]);
                std::process::exit(1);
            }
            let out_name = &args[3];
            
            let result = compile(&expr);
            let asm_program = format!(
                "section .text
global our_code_starts_here
our_code_starts_here:
{}
  ret
",
                result
            );

            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
        }
        "-e" => {
            // JIT compilation and execution only
            let mut ops = dynasmrt::x64::Assembler::new().unwrap();
            let start = ops.offset();
            compile_to_jit(&expr, &mut ops, &HashMap::new());
            dynasm!(ops ; .arch x64 ; ret);

            let buf = ops.finalize().unwrap();
            let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
            let result_val = jitted_fn();
            println!("{}", result_val);
        }
        "-g" => {
            // Both: JIT execution and AOT compilation
            if args.len() < 4 {
                eprintln!("Error: -g flag requires output file");
                eprintln!("Usage: {} -g <input.snek> <output.s>", args[0]);
                std::process::exit(1);
            }
            let out_name = &args[3];
            
            // JIT compilation and execution
            let mut ops = dynasmrt::x64::Assembler::new().unwrap();
            let start = ops.offset();
            compile_to_jit(&expr, &mut ops, &HashMap::new());
            dynasm!(ops ; .arch x64 ; ret);

            let buf = ops.finalize().unwrap();
            let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
            let result_val = jitted_fn();
            println!("{}", result_val);
            
            // AOT compilation to file
            let result = compile(&expr);
            let asm_program = format!(
                "section .text
global our_code_starts_here
our_code_starts_here:
{}
  ret
",
                result
            );

            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;
        }
        _ => {
            eprintln!("Error: Unknown flag '{}'", flag);
            eprintln!("Usage: {} <-c|-e|-g|-i> <input.snek> [output.s]", args[0]);
            std::process::exit(1);
        }
    }

    Ok(())
}