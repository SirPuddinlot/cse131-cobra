// src/compiler.rs
use im::HashMap;
use crate::ast::*;
use crate::instr::*;

static mut LABEL_COUNTER: i32 = 0;

fn new_label(prefix: &str) -> String {
    unsafe {
        LABEL_COUNTER += 1;
        format!("{}_{}", prefix, LABEL_COUNTER)
    }
}

fn compile_expr_value_to_tagged(expr: &Expr, defines: &HashMap<String, i32>) -> i32 {
    match expr {
        Expr::Number(n) => n << 1,
        Expr::Boolean(b) => if *b { TRUE_VAL } else { FALSE_VAL },
        Expr::Id(name) => *defines.get(name).expect("Unknown variable in define"),
        _ => panic!("Cannot evaluate expression at compile-time for top-level define"),
    }
}


const TRUE_VAL: i32 = 3;   // 0b11
const FALSE_VAL: i32 = 1;  // 0b01

pub fn compile_to_instrs(
    e: &Expr, si: i32, 
    env: &HashMap<String, i32>, 
    defines: &mut HashMap<String, i32>, 
    input: bool,
    loop_end: &Option<String> 
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))],
        Expr::Boolean(b) => {
            let val = if *b { TRUE_VAL } else { FALSE_VAL };
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(val))]
        }
        Expr::Input => {
            // Input comes in RDI register
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
        }
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
            let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
            match op {
                Op1::Add1 => {
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));

                    code.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                }
                Op1::Sub1 => {
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));

                    code.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1 << 1)));
                }
                Op1::IsNum => {
                    // Check LSB: if 0, it's a number
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    // If LSB = 0 (zero flag set), result is true; else false
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(TRUE_VAL)));
                    code.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                }
                Op1::IsBool => {
                    // Check LSB: if 1, it's a boolean
                    code.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    // If LSB = 1 (zero flag not set), result is true; else false
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::ICMovE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                }
            }
            code
        }
        Expr::BinOp(op, left, right) => {
            let mut code = compile_to_instrs(left, si, env, defines, input, loop_end);
            // Save left result on stack
            code.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            
            // Compile right
            let right_code = compile_to_instrs(right, si - 8, env, defines, input, loop_end);
            code.extend(right_code);
            
            match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    // Check both operands are numbers (LSB = 0 for both)
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IOr(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ITest(Val::Reg(Reg::RCX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    
                    match op {
                        Op2::Plus => {
                            code.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                            code.push(Instr::IJo("error_overflow".to_string()));
                        }
                        Op2::Minus => {
                            // Subtract: stack[si] - rax
                            code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                            code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                            code.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                            code.push(Instr::IJo("error_overflow".to_string()));
                        }
                        Op2::Times => {
                            // For multiplication, we need to shift right one operand
                            // to avoid double-tagging: (a << 1) * (b << 1) = (a * b) << 2
                            // We want (a * b) << 1, so we divide one by 2 first
                            code.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                            code.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                            code.push(Instr::IJo("error_overflow".to_string()));
                        }
                        _ => unreachable!(),
                    }
                }
                Op2::Less | Op2::Greater | Op2::LessEqual | Op2::GreaterEqual => {
                    // Check both operands are numbers (LSB = 0 for both)
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IOr(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ITest(Val::Reg(Reg::RCX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    
                    // Compare: stack[si] vs rax
                    code.push(Instr::ICmp(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(TRUE_VAL)));
                    
                    match op {
                        Op2::Less => code.push(Instr::ICMovL(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        Op2::Greater => code.push(Instr::ICMovG(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        Op2::LessEqual => code.push(Instr::ICMovLE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        Op2::GreaterEqual => code.push(Instr::ICMovGE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))),
                        _ => unreachable!(),
                    }
                }
                Op2::Equal => {
                    // Check both have same type (LSB must match)
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                    code.push(Instr::IXor(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::ITest(Val::Reg(Reg::RCX), Val::Imm(1)));
                    code.push(Instr::IJne("error_invalid_argument".to_string()));
                    
                    // Now compare values
                    code.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)));
                    code.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    code.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(FALSE_VAL)));
                    code.push(Instr::ICMovNE(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                }
            }
            code
        }
        // Expr::Set(name, expr) => {
        //     // Check if variable exists in environment
            
        //     if !env.contains_key(name) {
        //         // Also check if it's a define (for REPL)
        //         if !defines.contains_key(name) {
        //             panic!("Unbound variable identifier {}", name);
        //         }
        //         // Can't set! a define in this simple implementation
        //         panic!("Unbound variable identifier {}", name);
        //     }
            
        //     // Get the stack offset for this variable
        //     let offset = env.get(name).unwrap();
            
        //     // Compile the value expression
        //     let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
            
        //     // Store the result in the variable's location
        //     code.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
            
        //     // set! evaluates to the new value (which is already in RAX)
            
        //     code
        // }
        Expr::Set(name, expr) => {
            // Compile the value expression first
            let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
        
            if let Some(offset) = env.get(name) {
                // Local variable on stack → store the value at its offset
                code.push(Instr::IMov(Val::RegOffset(Reg::RSP, *offset), Val::Reg(Reg::RAX)));
            } 
            else if defines.contains_key(name) {
                // Top-level REPL define → update Rust-side map
                let value_tagged = compile_expr_value_to_tagged(expr, defines);
                // Update defines map (immutably, since you’re using im::HashMap)
                *defines = defines.update(name.clone(), value_tagged);
                // Optionally, also move RAX to some stack location if needed for JIT consistency
                // For example, push onto temporary stack slot:
                // code.push(Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RAX)));
            } 
            else {
                panic!("Unbound variable identifier {}", name);
            }
        
            // set! evaluates to the value in RAX already
            code
        }        
        Expr::If(cond, then_expr, else_expr) => {
            let else_label = new_label("else");
            let end_label = new_label("endif");
            
            let mut code = compile_to_instrs(cond, si, env, defines, input, loop_end);
            
            // If condition is false (FALSE_VAL = 3 = 0b11), jump to else
            // Any non-false value (including numbers) is treated as true
            code.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
            code.push(Instr::IJe(else_label.clone()));
            
            // Then branch
            let then_code = compile_to_instrs(then_expr, si, env, defines, input, loop_end);
            code.extend(then_code);
            code.push(Instr::IJmp(end_label.clone()));
            
            // Else branch
            code.push(Instr::ILabel(else_label));
            let else_code = compile_to_instrs(else_expr, si, env, defines, input, loop_end);
            code.extend(else_code);
            
            code.push(Instr::ILabel(end_label));
            code
        }
        Expr::Block(exprs) => {
            let mut code = Vec::new();
            for expr in exprs {
                let expr_code = compile_to_instrs(expr, si, env, defines, input, loop_end);
                code.extend(expr_code);
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
                let expr_code = compile_to_instrs(expr, current_si - 8, &new_env, defines, input, loop_end);
                code.extend(expr_code);
                
                // Store result on stack
                code.push(Instr::IMov(Val::RegOffset(Reg::RSP, current_si), Val::Reg(Reg::RAX)));
                new_env = new_env.update(name.clone(), current_si);
                current_si -= 8;
            }
            
            // Compile body with new environment
            let body_code = compile_to_instrs(body, current_si, &new_env, defines, input, loop_end);
            code.extend(body_code);
            
            code
        }
        // NEW: Loop implementation
        Expr::Loop(body) => {
            let loop_start = new_label("loop_start");
            let loop_end_label = new_label("loop_end");
            
            let mut code = Vec::new();
            code.push(Instr::ILabel(loop_start.clone()));
            
            // Compile body with loop_end context
            let body_code = compile_to_instrs(body, si, env, defines, input, &Some(loop_end_label.clone()));
            code.extend(body_code);
            
            // Jump back to start
            code.push(Instr::IJmp(loop_start));
            
            // End label (where break jumps to)
            code.push(Instr::ILabel(loop_end_label));
            
            code
        }
        
        // NEW: Break implementation
        Expr::Break(expr) => {
            // Check if we're inside a loop
            if loop_end.is_none() {
                panic!("break");
            }
            
            let loop_end_label = loop_end.as_ref().unwrap();
            
            // Compile the break value
            let mut code = compile_to_instrs(expr, si, env, defines, input, loop_end);
            
            // Jump to loop end with value in RAX
            code.push(Instr::IJmp(loop_end_label.clone()));
            
            code
        }
    }
}

pub fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e, -8, &HashMap::new(), &mut HashMap::new(), true, &None);
    let mut asm_code = String::new();
    
    for instr in instrs {
        asm_code.push_str(&instr_to_str(&instr));
        asm_code.push('\n');
    }
    asm_code.push_str("  ret\n");  // Add ret BEFORE error handlers
    
    // Add error handlers at the end
    asm_code.push_str("\nerror_overflow:\n");
    asm_code.push_str("  mov rdi, 1\n");
    asm_code.push_str("  call snek_error\n");
    asm_code.push_str("  ret\n");
    asm_code.push_str("\nerror_invalid_argument:\n");
    asm_code.push_str("  mov rdi, 2\n");
    asm_code.push_str("  call snek_error\n");
    asm_code.push_str("  ret\n");
    asm_code
}