use itertools::Itertools;
use std::cmp::{max, min};

type Range = (isize, isize);

#[derive(Debug)]
struct Region {
    range: Range3D,
    flipped_regions: Vec<Region>,
    overlaps_with: Vec<Range3D>,
}

impl Region {
    fn volume(&self) -> u128 {
        self.range.volume()
            - self
                .flipped_regions
                .iter()
                .map(|r| r.volume())
                .sum::<u128>()
            - self.overlaps_with.iter().map(|r| r.volume()).sum::<u128>()
    }
}

fn range_intersect((a1, a2): Range, (b1, b2): Range) -> Option<Range> {
    if a1 > b2 || a2 < b1 {
        None
    } else {
        Some((max(a1, b1), min(a2, b2)))
    }
}

#[derive(Debug, Copy, Clone)]
struct Range3D {
    x: Range,
    y: Range,
    z: Range,
}

impl Range3D {
    fn intersect(&self, other: &Self) -> Option<Self> {
        let x = range_intersect(self.x, other.x)?;
        let y = range_intersect(self.y, other.y)?;
        let z = range_intersect(self.z, other.z)?;
        Some(Self { x, y, z })
    }

    fn volume(&self) -> u128 {
        [self.x, self.y, self.z]
            .iter()
            .map(|(a, b)| (a - b).abs() as u128)
            .sum()
    }
}

#[derive(Debug)]
struct Instruction {
    on_or_off: bool,
    range: Range3D,
}

fn main() {
    let input = include_str!("../smallsample2");
    let instructions = input.lines().map(parse_line).collect_vec();
}

// "on x=-27877..-18112,y=70267..89349,z=24999..39366"
fn parse_line(s: &str) -> Instruction {
    let (on_string, rest) = s.split_once(" ").unwrap();
    let on_or_off = match on_string {
        "on" => true,
        "off" => false,
        otherwise => panic!("expected on/off but got {}", otherwise),
    };
    Instruction {
        on_or_off,
        range: parse_range(rest),
    }
}

fn parse_range(s: &str) -> Range3D {
    let (x, y, z) = s
        .split(",")
        .map(|range_s| range_s.split_once("=").unwrap().1.split_once("..").unwrap())
        .map(|(lo, hi)| (lo.parse().unwrap(), hi.parse().unwrap()))
        .map(|(lo, hi)| if hi < lo { (hi, lo) } else { (lo, hi) })
        .collect_tuple()
        .unwrap();
    Range3D { x, y, z }
}
