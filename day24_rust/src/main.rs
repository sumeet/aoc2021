use z3::ast::{Ast, Int};
use z3::{Optimize, Params};

#[derive(Debug, Copy, Clone)]
#[repr(usize)]
enum Register {
    W,
    X,
    Y,
    Z,
}

impl std::str::FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let c = s.chars().next().unwrap();
        Ok(match c.to_uppercase().next().unwrap() {
            'W' => Register::W,
            'X' => Register::X,
            'Y' => Register::Y,
            'Z' => Register::Z,
            otherwise => panic!("Invalid register: {}", otherwise),
        })
    }
}

#[derive(Debug)]
struct ALU<'ctx> {
    registers: [Int<'ctx>; 4],
    ctx: &'ctx z3::Context,
    optimizer: Optimize<'ctx>,
    all_inputs: Vec<Int<'ctx>>,
}

impl<'ctx> ALU<'ctx> {
    fn new(ctx: &'ctx z3::Context, solver: Optimize<'ctx>) -> Self {
        ALU {
            registers: [
                Int::from_i64(ctx, 0).into(),
                Int::from_i64(ctx, 0).into(),
                Int::from_i64(ctx, 0).into(),
                Int::from_i64(ctx, 0).into(),
            ],
            all_inputs: vec![],
            ctx,
            optimizer: solver,
        }
    }

    fn next_input(&mut self) -> Int<'ctx> {
        let next = Int::new_const(self.ctx, format!("w{}", self.all_inputs.len()));
        self.all_inputs.push(next.clone());
        self.optimizer.assert(&next.gt(&Int::from_i64(self.ctx, 0)));
        self.optimizer
            .assert(&next.lt(&Int::from_i64(self.ctx, 10)));
        next
    }

    fn run_instruction(&mut self, s: &str) {
        let mut parts = s.split_whitespace();
        let op = parts.next().unwrap();
        match op {
            "inp" => {
                let reg = parts.next().unwrap().parse().unwrap();
                let next_input = self.next_input();
                self.set_register(reg, next_input);
            }
            op @ _ => {
                let reg = parts.next().unwrap().parse().unwrap();
                let reg_value = self.get_register_value(reg);

                let next = parts.next().unwrap();
                let val = next
                    .parse::<i64>()
                    .ok()
                    .map(|v| Int::from_i64(self.ctx, v).into())
                    .unwrap_or_else(|| self.get_register_value(next.parse().unwrap()));

                self.set_register(
                    reg,
                    match op {
                        "add" => reg_value + val,
                        "mul" => reg_value * val,
                        "div" => reg_value / val,
                        "mod" => reg_value % val,
                        "eql" => reg_value
                            ._eq(&val)
                            .ite(&Int::from_i64(self.ctx, 1), &Int::from_i64(self.ctx, 0)),
                        otherwise => panic!("Invalid op: {}", otherwise),
                    },
                );
            }
        }
    }

    fn get_register_value(&self, reg: Register) -> Int<'ctx> {
        self.registers[reg as usize].clone()
    }

    fn set_register(&mut self, reg: Register, val: Int<'ctx>) {
        self.registers[reg as usize] = val;
    }
}

fn main() {
    let input = include_str!("../input");
    let mut config = z3::Config::new();
    config.set_proof_generation(true);
    let ctx = z3::Context::new(&config);
    let optimizer = Optimize::new(&ctx);
    let mut alu = ALU::new(&ctx, optimizer);
    for line in input.lines() {
        println!("{}", line);
        alu.run_instruction(line);
    }

    let mut input = Int::from_i64(&ctx, 0);
    for i in &alu.all_inputs {
        input *= Int::from_i64(&ctx, 10);
        input += i;
    }

    alu.optimizer
        .assert(&alu.registers[Register::Z as usize]._eq(&Int::from_i64(&ctx, 0)));
    alu.optimizer.maximize(&input);
    dbg!(alu.optimizer.check(&[]));
    dbg!(alu.optimizer.get_model());
}
