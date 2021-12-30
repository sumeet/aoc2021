#![feature(box_syntax)]

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
struct ALU {
    registers: [Value; 4],
}

impl ALU {
    fn new() -> Self {
        ALU {
            registers: [
                Value::Known(0),
                Value::Known(0),
                Value::Known(0),
                Value::Known(0),
            ],
        }
    }

    fn run_instruction(&mut self, s: &str) {
        let mut parts = s.split_whitespace();
        let op = parts.next().unwrap();
        match op {
            "inp" => {
                let reg = parts.next().unwrap().parse().unwrap();
                self.set_register(reg, Value::Input);
            }
            op @ _ => {
                let reg = parts.next().unwrap().parse().unwrap();
                let reg_value = self.get_register_value(reg);

                let next = parts.next().unwrap();
                let val = Value::parse_num(next)
                    .unwrap_or_else(|| self.get_register_value(next.parse().unwrap()));

                self.set_register(
                    reg,
                    match op {
                        "add" => reg_value + val,
                        "mul" => reg_value * val,
                        "div" => reg_value / val,
                        "mod" => reg_value % val,
                        "eql" => reg_value.eql(val),
                        otherwise => panic!("Invalid op: {}", otherwise),
                    },
                );
            }
        }
    }

    fn get_register_value(&self, reg: Register) -> Value {
        self.registers[reg as usize].clone()
    }

    fn set_register(&mut self, reg: Register, val: Value) {
        self.registers[reg as usize] = val;
    }
}

#[derive(Debug, Clone)]
enum Value {
    Known(isize),
    Input,
    Add(Box<Value>, Box<Value>),
    Mul(Box<Value>, Box<Value>),
    Mod(Box<Value>, Box<Value>),
    Div(Box<Value>, Box<Value>),
    Eql(Box<Value>, Box<Value>),
}

impl Value {
    fn parse_num(s: &str) -> Option<Self> {
        s.parse::<isize>().ok().map(Value::Known)
    }

    fn eql(self, other: Value) -> Self {
        match (self, other) {
            (Value::Known(a), Value::Known(b)) => Value::Known(if a == b { 1 } else { 0 }),
            (l, r) => Value::Eql(Box::new(l), Box::new(r)),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Known(0), r) => r,
            (l, Value::Known(0)) => l,
            (Value::Known(a), Value::Known(b)) => Value::Known(a + b),
            (l, r) => Value::Add(box l, box r),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Value;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Known(a), Value::Known(b)) => Value::Known(a * b),
            (Value::Known(1), r) => r,
            (l, Value::Known(1)) => l,
            (Value::Known(0), _) => Value::Known(0),
            (_, Value::Known(0)) => Value::Known(0),
            (l, r) => Value::Mul(box l, box r),
        }
    }
}

impl std::ops::Rem for Value {
    type Output = Value;

    fn rem(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Known(a), Value::Known(b)) => Value::Known(a % b),
            (l, r) => Value::Mod(box l, box r),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Value;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Known(a), Value::Known(b)) => Value::Known(a / b),
            (l, Value::Known(1)) => l,
            (l, r) => Value::Div(box l, box r),
        }
    }
}

fn main() {
    let input = include_str!("../input");
    let mut alu = ALU::new();
    for line in input.lines() {
        println!("{}", line);
        alu.run_instruction(line);
    }
    dbg!(&alu.registers[Register::Z as usize]);
}
