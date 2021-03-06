use dynasm::dynasm;
use dynasmrt::DynasmApi;
use radixal::IntoDigits;

use rayon::prelude::{IntoParallelIterator, ParallelIterator};

fn to_rq(reg: char) -> u8 {
    match reg {
        'w' => 8,
        'x' => 9,
        'y' => 10,
        'z' => 11,
        _ => panic!("Unknown register"),
    }
}

#[derive(PartialEq)]
enum Val {
    Imm(i32),
    Reg(u8),
}

fn parse_val(s: &str) -> Val {
    let first_char = s.chars().next().unwrap();
    if ['w', 'x', 'y', 'z'].contains(&first_char) {
        Val::Reg(to_rq(first_char))
    } else {
        Val::Imm(s.parse().unwrap())
    }
}

fn main() {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();

    // initialize all registers to 0
    dynasm!(ops
        ; xor Rq(to_rq('w')), Rq(to_rq('w'))
        ; xor Rq(to_rq('x')), Rq(to_rq('x'))
        ; xor Rq(to_rq('y')), Rq(to_rq('y'))
        ; xor Rq(to_rq('z')), Rq(to_rq('z'))
    );

    let input = include_str!("../input");
    for line in input.lines() {
        let mut split = line.split_whitespace();
        let op = split.next().unwrap();
        match op {
            "inp" => {
                // grab the least significant dec of the input num (it's backwards)
                let reg = to_rq(split.next().unwrap().chars().next().unwrap());
                dynasm!(ops
                    ; mov rax, rdi
                    ; cqo
                    ; mov rcx, 10
                    ; idiv rcx
                    ; mov rdi, rax
                    ; mov Rq(reg), rdx
                );
            }
            "add" => {
                let reg = to_rq(split.next().unwrap().chars().next().unwrap());
                let val = parse_val(split.next().unwrap());
                match val {
                    // this is a no-op, let's not compile it in
                    Val::Imm(0) => continue,
                    Val::Imm(n) => {
                        dynasm!(ops
                            ; add Rq(reg), n
                        )
                    }
                    Val::Reg(reg2) => {
                        dynasm!(ops
                            ; add Rq(reg), Rq(reg2)
                        )
                    }
                }
            }
            "mul" => {
                let reg = to_rq(split.next().unwrap().chars().next().unwrap());
                let val = parse_val(split.next().unwrap());
                match val {
                    // this is a no-op, let's not compile it in
                    Val::Imm(1) => continue,
                    // multiplying by 0, well...
                    Val::Imm(0) => {
                        dynasm!(ops
                            ; xor Rq(reg), Rq(reg)
                        )
                    }
                    Val::Imm(n) => {
                        dynasm!(ops
                            ; mov rax, n
                            ; cqo
                            ; imul Rq(reg)
                            ; mov Rq(reg), rax
                        )
                    }
                    Val::Reg(reg2) => {
                        dynasm!(ops
                            ; mov rax, Rq(reg2)
                            ; cqo
                            ; imul Rq(reg)
                            ; mov Rq(reg), rax
                        )
                    }
                }
            }
            "mod" => {
                let reg = to_rq(split.next().unwrap().chars().next().unwrap());
                let val = parse_val(split.next().unwrap());
                match val {
                    Val::Imm(n) => {
                        dynasm!(ops
                            ; mov rax, Rq(reg)
                            ; cqo
                            ; mov rcx, n
                            ; idiv rcx
                            ; mov Rq(reg), rdx
                        )
                    }
                    Val::Reg(reg2) => {
                        dynasm!(ops
                            ; mov rax, Rq(reg)
                            ; cqo
                            ; idiv Rq(reg2)
                            ; mov Rq(reg), rdx
                        )
                    }
                }
            }
            "div" => {
                let reg = to_rq(split.next().unwrap().chars().next().unwrap());
                let val = parse_val(split.next().unwrap());
                match val {
                    // dividing by 1...
                    Val::Imm(1) => continue,
                    Val::Imm(n) => {
                        dynasm!(ops
                            ; mov rax, Rq(reg)
                            ; cqo
                            ; mov rcx, n
                            ; idiv rcx
                            ; mov Rq(reg), rax
                        )
                    }
                    Val::Reg(reg2) => {
                        dynasm!(ops
                            ; mov rax, Rq(reg)
                            ; cqo
                            ; idiv Rq(reg2)
                            ; mov Rq(reg), rax
                        )
                    }
                }
            }
            "eql" => {
                let reg = to_rq(split.next().unwrap().chars().next().unwrap());
                let val = parse_val(split.next().unwrap());
                match val {
                    Val::Imm(n) => {
                        dynasm!(ops
                            ; cmp Rq(reg), n
                            ; sete al
                            ; movzx Rq(reg), al
                        )
                    }
                    Val::Reg(reg2) => {
                        dynasm!(ops
                            ; cmp Rq(reg), Rq(reg2)
                            ; sete al
                            ; movzx Rq(reg), al
                        )
                    }
                }
            }
            otherwise => panic!("invalid op: {}", otherwise),
        }
    }
    dynasm!(ops
        ; mov rax, Rq(to_rq('z'))
        ; ret
    );

    let exec_buf = ops.finalize().unwrap();
    let func = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(exec_buf.as_ptr()) };

    let nums_to_test = 11_111_111_111_111i64..=99_999_999_999_999i64;
    nums_to_test
        .into_par_iter()
        .filter(|n| func(*n) == 0)
        .for_each(|n| println!("{}", n));
}
