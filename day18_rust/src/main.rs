#![feature(box_patterns)]
#![feature(box_syntax)]

use std::iter::Peekable;
use std::thread::sleep;

#[derive(Debug, Clone, PartialEq)]
enum Pair {
    Scalar(usize),
    Pair(Box<Pair>, Box<Pair>),
    LeftAnd(Box<Pair>, usize),
    ToTheRight(Box<Pair>, usize),
    RightAnd(Box<Pair>, usize),
    ToTheLeft(Box<Pair>, usize),
}

#[derive(Debug)]
enum ReduceResult {
    Reduced,
    Untouched,
}

fn dump(p: &Pair) -> String {
    match p {
        Pair::Scalar(n) => n.to_string(),
        Pair::Pair(l, r) => format!("[{},{}]", dump(l), dump(r)),
        Pair::LeftAnd(p, n) => format!("LeftAnd(n: {}, {})", n, dump(p)),
        Pair::ToTheRight(p, n) => format!("ToTheRight(n: {}, {})", n, dump(p)),
        Pair::RightAnd(p, n) => format!("RightAnd(n: {}, {})", n, dump(p)),
        Pair::ToTheLeft(p, n) => format!("ToTheLeft(n: {}, {})", n, dump(p)),
    }
}

fn until_stable<T: PartialEq + Clone>(mut func: impl FnMut(T) -> T, t: T) -> T {
    let mut prev = t;
    loop {
        let curr = func(prev.clone());
        if curr == prev {
            return curr;
        }
        prev = curr;
    }
}

fn div_round_down(a: usize, b: usize) -> usize {
    a / b
}

fn div_round_up(a: usize, b: usize) -> usize {
    (a + b - 1) / b
}

fn reduce(pair: Pair, depth: usize) -> (Pair, ReduceResult) {
    use crate::Pair::*;
    use ReduceResult::*;
    match pair {
        Scalar(n) => {
            if n >= 10 {
                (
                    Pair(
                        Box::new(Scalar(div_round_down(n, 2))),
                        Box::new(Scalar(div_round_up(n, 2))),
                    ),
                    Reduced,
                )
            } else {
                (Scalar(n), Untouched)
            }
        }
        // left and to the right
        Pair(box Scalar(n), box LeftAnd(r, explode_n)) => {
            (Pair(Box::new(Scalar(n + explode_n)), r), Reduced)
        }
        Pair(box Pair(pl, pr), box LeftAnd(r, explode_n)) => (
            Pair(box Pair(box ToTheRight(pl, explode_n), pr), r),
            Reduced,
        ),
        Pair(box ToTheRight(l, explode_n), box Pair(rl, rr)) => (
            Pair(l, box Pair(box ToTheRight(rl, explode_n), rr)),
            Reduced,
        ),
        Pair(box ToTheRight(l, explode_n), box Scalar(n)) => {
            (Pair(l, box Scalar(n + explode_n)), Reduced)
        }
        // right and to the left
        Pair(box RightAnd(l, explode_n), box Scalar(n)) => {
            (Pair(l, box Scalar(n + explode_n)), Reduced)
        }
        Pair(box RightAnd(l, explode_n), box Pair(rl, rr)) => {
            (Pair(l, box Pair(rl, box ToTheLeft(rr, explode_n))), Reduced)
        }
        Pair(l, box RightAnd(r, explode_n)) => ((RightAnd(box Pair(l, r), explode_n), Reduced)),
        Pair(box Scalar(n), box ToTheLeft(r, explode_n)) => {
            (Pair(box Scalar(n + explode_n), r), Reduced)
        }
        Pair(box Pair(ll, lr), box ToTheLeft(r, explode_n)) => {
            (Pair(box Pair(ll, box ToTheLeft(lr, explode_n)), r), Reduced)
        }
        // do the reduction, we may need to repeatedly call this
        Pair(l, r) => {
            let (l_reduced, was_reduced) = reduce(*l, depth + 1);
            match was_reduced {
                Reduced => (Pair(Box::new(l_reduced), r), Reduced),
                Untouched => {
                    let (r_reduced, was_reduced) = reduce(*r, depth + 1);
                    (Pair(Box::new(l_reduced), Box::new(r_reduced)), was_reduced)
                }
            }
        }
        RightAnd(inner, explode_n) => {
            if depth == 0 {
                (*inner, Reduced)
            } else {
                (RightAnd(inner, explode_n), Reduced)
            }
        }
        LeftAnd(inner, explode_n) => {
            if depth == 0 {
                (*inner, Reduced)
            } else {
                (LeftAnd(inner, explode_n), Reduced)
            }
        }
        _ => panic!("unexpected pair: {:?}", pair),
    }
}

fn explode(l: usize, r: usize) -> Pair {
    use crate::Pair::*;
    RightAnd(box LeftAnd(box Scalar(0), l), r)
}

fn parse_num(chars: &mut Peekable<impl Iterator<Item = char>>) -> usize {
    let mut n = 0;
    while let Some(c) = chars.peek() {
        if c.is_digit(10) {
            n = n * 10 + c.to_digit(10).unwrap();
            chars.next();
        } else {
            break;
        }
    }
    n as _
}

fn parse(chars: &mut Peekable<impl Iterator<Item = char>>) -> Pair {
    let peeked = chars.peek().unwrap().clone();
    if peeked.is_digit(10) {
        Pair::Scalar(parse_num(chars))
    } else if peeked == 'E' {
        chars.next(); // consume the 'E'
        chars.next(); // consume the '['
        let l = parse_num(chars);
        chars.next(); // consume the ','
        let r = parse_num(chars);
        chars.next(); // consume the ']'
        explode(l, r)
    } else if peeked == '[' {
        chars.next(); // consume the '['
        let a = parse(chars);
        chars.next(); // consume the ','
        let b = parse(chars);
        chars.next(); // consume the ']'
        Pair::Pair(Box::new(a), Box::new(b))
    } else {
        panic!("invalid str: {:?}", peeked);
    }
}

fn main() {
    let init_pair = parse(&mut "[[[[E[9,8],1],2],3],4]".chars().peekable());
    let stabilized = until_stable(
        |pair| {
            println!("{}", dump(&pair));
            sleep(std::time::Duration::from_millis(1000));
            reduce(pair, 0).0
        },
        init_pair,
    );
    println!("{}", dump(&stabilized));
}
