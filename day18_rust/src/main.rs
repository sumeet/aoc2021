#![feature(box_patterns)]
#![feature(box_syntax)]

use itertools::Itertools;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
enum Pair {
    Scalar(usize),
    Pair(Box<Pair>, Box<Pair>),
    LeftAnd(Box<Pair>, usize),
    ToTheRight(Box<Pair>, usize),
    RightAnd(Box<Pair>, usize),
    ToTheLeft(Box<Pair>, usize),
}

fn magnitude(pair: &Pair) -> usize {
    match pair {
        Pair::Scalar(magnitude) => *magnitude,
        Pair::Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right),
        _ => panic!("unexpected"),
    }
}

#[derive(Debug)]
enum ReduceResult {
    Reduced,
    Untouched,
}

#[allow(unused)]
fn dump(p: &Pair) -> String {
    match p {
        Pair::Scalar(n) => n.to_string(),
        Pair::Pair(l, r) => format!("[{},{}]", dump(l), dump(r)),
        Pair::LeftAnd(p, n) => format!("<{}({})", n, dump(p)),
        Pair::ToTheRight(p, n) => format!("<{}({})", n, dump(p)),
        Pair::RightAnd(p, n) => format!(">{}({})", n, dump(p)),
        Pair::ToTheLeft(p, n) => format!(">{}({})", n, dump(p)),
    }
}

fn left_most(p: &mut Pair) -> &mut Pair {
    match p {
        Pair::Pair(l, _) => left_most(l),
        _ => p,
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

fn trigger_explosion(pair: Pair, depth: usize) -> Option<Pair> {
    use crate::Pair::*;
    match pair {
        // explosion case
        Pair(box Scalar(l), box Scalar(r)) if depth > 3 => Some(explode(l, r)),
        Pair(box l, box r) => match trigger_explosion(l.clone(), depth + 1) {
            Some(left_explosion) => Some(Pair(box left_explosion, box r)),
            None => match trigger_explosion(r, depth + 1) {
                Some(right_explosion) => Some(Pair(box l, box right_explosion)),
                None => None,
            },
        },
        Scalar(_) => None,
        otherwise => panic!(
            "can't trigger explosion with ongoing activity: {:?}",
            otherwise
        ),
    }
}

fn trigger_split(pair: Pair) -> Option<Pair> {
    use crate::Pair::*;
    match pair {
        Pair(box l, box r) => match trigger_split(l.clone()) {
            Some(left_split) => Some(Pair(box left_split, box r)),
            None => match trigger_split(r) {
                Some(right_split) => Some(Pair(box l, box right_split)),
                None => None,
            },
        },
        Scalar(n) => {
            if n >= 10 {
                Some(Pair(
                    Box::new(Scalar(div_round_down(n, 2))),
                    Box::new(Scalar(div_round_up(n, 2))),
                ))
            } else {
                None
            }
        }
        otherwise => panic!("can't trigger split with ongoing activity: {:?}", otherwise),
    }
}

fn reduce_explosion(pair: Pair, depth: usize) -> (Pair, ReduceResult) {
    use crate::Pair::*;
    use ReduceResult::*;
    let (mut res, was_reduced) = match pair {
        // left explode propagates left and to the right
        Pair(box Scalar(n), box LeftAnd(r, explode_n)) => {
            (Pair(Box::new(Scalar(n + explode_n)), r), Reduced)
        }
        Pair(box Pair(pl, pr), box LeftAnd(r, explode_n)) => (
            Pair(box Pair(box ToTheRight(pl, explode_n), pr), r),
            Reduced,
        ),
        Pair(box LeftAnd(l, explode_n), pr) => (LeftAnd(box Pair(l, pr), explode_n), Reduced),
        Pair(box ToTheRight(l, explode_n), box Pair(rl, rr)) => (
            Pair(l, box Pair(box ToTheRight(rl, explode_n), rr)),
            Reduced,
        ),
        Pair(box ToTheRight(l, explode_n), box Scalar(n)) => {
            (Pair(l, box Scalar(n + explode_n)), Reduced)
        }
        // right explode propagates right and to the left
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
            let (l_reduced, was_reduced) = reduce_explosion(*l, depth + 1);
            match was_reduced {
                Reduced => (Pair(Box::new(l_reduced), r), Reduced),
                Untouched => {
                    let (r_reduced, was_reduced) = reduce_explosion(*r, depth + 1);
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
        Scalar(n) => (Scalar(n), Untouched),
        _ => panic!("unexpected pair: {:?}", pair),
    };
    if depth == 0 {
        let left_most_el = left_most(&mut res);
        let inner_to_replace = match left_most_el {
            LeftAnd(p, _) => Some(p.clone()),
            _ => None,
        };
        if let Some(inner_to_replace) = inner_to_replace {
            *left_most_el = *inner_to_replace;
        }
    }
    (res, was_reduced)
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

fn reduce(p: Pair) -> Pair {
    let exploded = trigger_explosion(p.clone(), 0)
        .map(|exploded| until_stable(|pair| reduce_explosion(pair, 0).0, exploded));
    exploded.or_else(|| trigger_split(p.clone())).unwrap_or(p)
}

fn reduce_until_stable(p: Pair) -> Pair {
    until_stable(|pair| reduce(pair), p)
}

fn add(l: Pair, r: Pair) -> Pair {
    Pair::Pair(Box::new(l), Box::new(r))
}

fn main() {
    part1();
    part2();
}

fn part1() {
    let mut pairs = parse_lines(include_str!("../input"));
    let mut res = reduce(pairs.next().unwrap());
    for pair in pairs {
        res = reduce_until_stable(add(res, pair));
    }
    println!("part 1: {}", magnitude(&res));
}

fn part2() {
    let pairs = parse_lines(include_str!("../input")).collect_vec();
    let max = pairs
        .into_iter()
        .tuple_combinations()
        .map(|(a, b)| magnitude(&reduce_until_stable(add(a, b))))
        .max();
    println!("part 2: {}", max.unwrap());
}

fn parse_lines(s: &str) -> impl Iterator<Item = Pair> + '_ {
    s.lines().map(|line| parse(&mut line.chars().peekable()))
}
