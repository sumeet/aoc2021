use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::HashSet;
use std::ops::{Add, Neg, Sub};

lazy_static! {
    static ref ARRANGEMENTS: Vec<Arrangement> = {
        Dim::all_combos()
            .into_iter()
            .cartesian_product(all_flips().into_iter())
            .map(|(dims, flips)| Arrangement { dims, flips })
            .collect()
    };
}

// returned from target perspective
fn find_matches(target: &Scanner, cand: &Scanner) -> Option<(impl Fn(Coord) -> Coord, Coord)> {
    target
        .all_arrangements()
        .into_iter()
        .cartesian_product(cand.all_arrangements().into_iter())
        .find_map(|((arrange_a, a_beacons), (arrange_b, b_beacons))| {
            a_beacons
                .into_iter()
                .cartesian_product(b_beacons.into_iter())
                .into_group_map_by(|(a, b)| (*a - *b))
                .into_iter()
                .find_map(|(diff, a_and_b_coords)| {
                    if a_and_b_coords.len() >= 12 {
                        Some((
                            move |coord: Coord| {
                                (diff + coord.in_arrangement(arrange_b))
                                    .reverse_arrangement(arrange_a)
                            },
                            diff.reverse_arrangement(arrange_a),
                        ))
                    } else {
                        None
                    }
                })
        })
}

fn manhattan_distance(a: Coord, b: Coord) -> isize {
    (a.x - b.x).abs() + (a.y - b.y).abs() + (a.z - b.z).abs()
}

fn main() {
    let parsed = parse(include_str!("../input"));

    let mut coord_counts: HashSet<Coord> = parsed[0].beacons.iter().copied().collect();
    let mut scanner_locs = HashSet::new();
    scanner_locs.insert(Coord { x: 0, y: 0, z: 0 });

    let mut q = parsed[1..].iter().cloned().collect_vec();
    while !q.is_empty() {
        let mut next_q = vec![];
        for next_scanner in &q {
            let our_scanner = Scanner {
                beacons: coord_counts.iter().cloned().collect(),
            };
            if let Some((coord_fn, scanner_loc)) = find_matches(&our_scanner, &next_scanner) {
                scanner_locs.insert(scanner_loc);
                for coord in next_scanner.beacons.iter().copied().map(coord_fn) {
                    coord_counts.insert(coord);
                }
                continue; // without reinserting
            }
            // we'll try again next time
            next_q.push(next_scanner.clone());
        }
        q = next_q;
    }

    println!("Part 1: {}", coord_counts.len());
    let max_distance = scanner_locs
        .iter()
        .tuple_combinations()
        .map(|(a, b)| manhattan_distance(*a, *b))
        .max()
        .unwrap();
    println!("Part 2: {}", max_distance);
}

#[derive(Clone, Copy, Debug)]
struct Arrangement {
    dims: [Dim; 3],
    flips: [isize; 3],
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Dim {
    X,
    Y,
    Z,
}

fn all_flips() -> Vec<[isize; 3]> {
    [-1isize, 1isize]
        .iter()
        .copied()
        .combinations_with_replacement(3)
        .map(|v| [v[0], v[1], v[2]])
        .collect()
}

impl Dim {
    fn all_combos() -> Vec<[Dim; 3]> {
        use Dim::*;
        [X, Y, Z]
            .iter()
            .cloned()
            .permutations(3)
            .unique()
            .map(|v| [v[0], v[1], v[2]])
            .collect()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Coord {
    x: isize,
    y: isize,
    z: isize,
}

impl Sub for Coord {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Coord {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl Add for Coord {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Coord {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl Neg for Coord {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Coord {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

impl Coord {
    fn new(x: isize, y: isize, z: isize) -> Coord {
        Coord { x, y, z }
    }

    fn get_dim(&self, dim: Dim) -> isize {
        match dim {
            Dim::X => self.x,
            Dim::Y => self.y,
            Dim::Z => self.z,
        }
    }

    fn in_arrangement(self, Arrangement { dims, flips }: Arrangement) -> Self {
        let (x, y, z) = dims
            .iter()
            .zip(flips.iter())
            .map(|(dim, flip)| self.get_dim(*dim) * flip)
            .collect_tuple()
            .unwrap();
        Self { x, y, z }
    }

    fn reverse_arrangement(self, Arrangement { dims, flips }: Arrangement) -> Self {
        let mut xyz = [0; 3];
        for (i, dim) in dims.iter().enumerate() {
            xyz[i] = self.get_dim(*dim)
                * flips[match dim {
                    Dim::X => 0,
                    Dim::Y => 1,
                    Dim::Z => 2,
                }]
        }
        Self {
            x: xyz[0],
            y: xyz[1],
            z: xyz[2],
        }
    }
}

#[derive(Debug, Clone)]
struct Scanner {
    beacons: Vec<Coord>,
}

impl Scanner {
    fn new() -> Self {
        Scanner {
            beacons: Vec::new(),
        }
    }

    fn all_arrangements(&self) -> Vec<(Arrangement, Vec<Coord>)> {
        ARRANGEMENTS
            .iter()
            .copied()
            .map(|arrangement| {
                (
                    arrangement,
                    self.beacons
                        .iter()
                        .map(|loc| loc.in_arrangement(arrangement))
                        .collect(),
                )
            })
            .collect()
    }
}

fn parse(s: &str) -> Vec<Scanner> {
    let mut lines = s.lines();
    let mut ret = vec![];
    // skip the first line --- scanner 0 ---
    lines.next().unwrap();
    let mut this_scanner = Scanner::new();
    while let Some(line) = lines.next() {
        if line.is_empty() {
            // skip over the next line which is the separator
            lines.next();
            this_scanner = {
                ret.push(this_scanner);
                Scanner::new()
            };
            continue;
        }

        let (x, y, z) = line
            .split(",")
            .map(|s| s.parse().unwrap())
            .collect_tuple()
            .unwrap();
        this_scanner.beacons.push(Coord::new(x, y, z));
    }
    ret.push(this_scanner);
    ret
}
