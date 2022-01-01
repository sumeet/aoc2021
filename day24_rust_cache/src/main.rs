use dashmap::DashMap;
use lazy_static::lazy_static;
use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};

lazy_static! {
    static ref INSTRS: Vec<Instr> = include_str!("../input").lines().map(parse_instr).collect();
    // part 1
    // static ref ORDER: Vec<i64> = (1..=9).rev().collect();
    // part 2
    static ref ORDER: Vec<i64> = (1..=9).collect();
    static ref CACHE: DashMap<(usize, State), (bool, i64)> = DashMap::new();
}

fn run(idx: usize, mut state: State) -> (bool, i64) {
    if let Some(res) = CACHE.get(&(idx, state)) {
        return *res;
    }

    if idx >= INSTRS.len() {
        let res = state[3] == 0;
        CACHE.insert((idx, state), (res, 0));
        return (res, 0);
    }

    let res = match INSTRS[idx] {
        Instr::Input(i) => {
            let res = match ORDER.par_iter().find_map_first(move |n| {
                let mut state = state.clone();
                state[i as usize] = *n;
                let (matched, rest) = run(idx + 1, state);
                if matched {
                    Some((rest * 10) + *n)
                } else {
                    None
                }
            }) {
                Some(n) => (true, n),
                None => (false, 0),
            };
            CACHE.insert((idx, state), res);
            res
        }
        Instr::Add(i, n) => {
            state[i as usize] += bind(state, n);
            run(idx + 1, state)
        }
        Instr::Mul(i, n) => {
            state[i as usize] *= bind(state, n);
            run(idx + 1, state)
        }
        Instr::Mod(i, n) => {
            state[i as usize] %= bind(state, n);
            run(idx + 1, state)
        }
        Instr::Div(i, n) => {
            state[i as usize] /= bind(state, n);
            run(idx + 1, state)
        }
        Instr::Eql(i, n) => {
            state[i as usize] = (state[i as usize] == bind(state, n)).into();
            run(idx + 1, state)
        }
    };
    res
}

fn main() {
    let (success, n) = run(0, [0; 4]);
    println!(
        "{}, {}",
        success,
        n.to_string().chars().rev().collect::<String>()
    );
}

type State = [i64; 4];

fn bind(state: State, val: Val) -> i64 {
    match val {
        Val::Reg(i) => state[i as usize],
        Val::Num(i) => i,
    }
}

#[derive(Copy, Clone, Debug)]
enum Val {
    Num(i64),
    Reg(u8),
}

fn parse_val(s: &str) -> Val {
    s.parse()
        .map(Val::Num)
        .unwrap_or_else(|_| Val::Reg(parse_reg(s)))
}

fn parse_reg(s: &str) -> u8 {
    s.chars().next().unwrap() as u8 - 'w' as u8
}

#[derive(Debug, Copy, Clone)]
enum Instr {
    Input(u8),
    Add(u8, Val),
    Mul(u8, Val),
    Mod(u8, Val),
    Div(u8, Val),
    Eql(u8, Val),
}

fn parse_instr(s: &str) -> Instr {
    let mut chars = s.split_whitespace();
    match chars.next().unwrap() {
        "inp" => Instr::Input(parse_reg(chars.next().unwrap())),
        "add" => Instr::Add(
            parse_reg(chars.next().unwrap()),
            parse_val(chars.next().unwrap()),
        ),
        "mul" => Instr::Mul(
            parse_reg(chars.next().unwrap()),
            parse_val(chars.next().unwrap()),
        ),
        "mod" => Instr::Mod(
            parse_reg(chars.next().unwrap()),
            parse_val(chars.next().unwrap()),
        ),
        "div" => Instr::Div(
            parse_reg(chars.next().unwrap()),
            parse_val(chars.next().unwrap()),
        ),
        "eql" => Instr::Eql(
            parse_reg(chars.next().unwrap()),
            parse_val(chars.next().unwrap()),
        ),
        _ => panic!("invalid instruction: {}", s),
    }
}
