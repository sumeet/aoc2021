use lazy_static::lazy_static;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use std::cmp::max;
use std::ops::RangeInclusive;

#[allow(unused)]
const SAMPLE: &str = "target area: x=20..30, y=-10..-5";
#[allow(unused)]
const INPUT: &str = "target area: x=244..303, y=-91..-54";
lazy_static! {
    static ref RANGES: (RangeInclusive<isize>, RangeInclusive<isize>) = parse(INPUT);
    static ref X_RANGE: RangeInclusive<isize> = RANGES.0.clone();
    static ref Y_RANGE: RangeInclusive<isize> = RANGES.1.clone();
}

fn main() {
    let part1 = (-10000..=10000)
        .into_par_iter()
        .filter_map(|x| {
            (-10000..=10000)
                .into_par_iter()
                .filter_map(|y| check(x, y))
                .max()
        })
        .max()
        .unwrap();
    dbg!(part1);
}

fn check(x: isize, y: isize) -> Option<isize> {
    let mut max_y = isize::MIN;
    for state in step_n(State::new(x, y), 1000) {
        max_y = max(max_y, state.pos.y);
        if X_RANGE.contains(&state.pos.x) && Y_RANGE.contains(&state.pos.y) {
            return Some(max_y);
        }
    }
    None
}

impl State {
    fn new(x: isize, y: isize) -> Self {
        Self {
            vel: Xy { x, y },
            pos: Xy { x: 0, y: 0 },
        }
    }
}

fn step_n(mut state: State, n: usize) -> impl Iterator<Item = State> {
    (0..n).into_iter().map(move |_| {
        state = step(state);
        state
    })
}

fn step(mut state: State) -> State {
    state.pos.x += state.vel.x;
    state.pos.y += state.vel.y;
    state.vel.x = match state.vel.x {
        0 => state.vel.x,
        _ if state.vel.x < 0 => state.vel.x + 1,
        _ => state.vel.x - 1,
    };
    state.vel.y -= 1;
    state
}

#[derive(Copy, Clone, Debug)]
struct State {
    pos: Xy,
    vel: Xy,
}

#[derive(Copy, Clone, Debug)]
struct Xy {
    x: isize,
    y: isize,
}

fn parse(s: &str) -> (RangeInclusive<isize>, RangeInclusive<isize>) {
    let mut pieces = s.split(" ");
    pieces.next().unwrap();
    pieces.next().unwrap();
    let x_range_str: String = pieces
        .next()
        .unwrap()
        .split(",")
        .next()
        .unwrap()
        .chars()
        .skip(2)
        .collect();
    let y_range_str: String = pieces
        .next()
        .unwrap()
        .split(",")
        .next()
        .unwrap()
        .chars()
        .skip(2)
        .collect();

    fn str_to_range(s: &str) -> RangeInclusive<isize> {
        let mut nums = s
            .split("..")
            .map(|ns| ns.parse().unwrap())
            .collect::<Vec<_>>();
        nums.sort();
        nums[0]..=nums[1]
    }
    (str_to_range(&x_range_str), str_to_range(&y_range_str))
}
