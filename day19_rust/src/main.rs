use itertools::Itertools;
use lazy_static::lazy_static;
use std::ops::Sub;

lazy_static! {
    static ref ARRANGEMENTS: Vec<Arrangement> = {
        Dim::all_combos()
            .into_iter()
            .cartesian_product(all_flips().into_iter())
            .map(|(dims, flips)| Arrangement { dims, flips })
            .collect()
    };
}

fn main() {
    let parsed = parse(include_str!("../sample"));
    let comms = find_commonalities(&parsed);
    let comm = &comms[0];
    assert_eq!(comm.a_idx, 0);
    assert_eq!(comm.b_idx, 1);

    let arr1 = comm.a_arr;
    // let arr2 = comm.b_arr;
    let some_beacon = comm.b_beacons[0];
    let there = some_beacon.in_arrangement(arr1);
    let there_and_back = there.reverse_arrangement(arr1);
    dbg!(arr1);
    dbg!(there);
    assert_eq!(some_beacon, there_and_back);
    // dbg!(comm
    //     .b_beacons
    //     .iter()
    //     .map(|beacon| beacon.reverse_arrangement(arr1).reverse_arrangement(arr2))
    //     .collect_vec());
}

#[derive(Debug)]
struct ScannerMatch {
    a_idx: usize,
    a_arr: Arrangement,
    a_beacons: Vec<Coord>,
    b_idx: usize,
    b_arr: Arrangement,
    b_beacons: Vec<Coord>,
}

fn find_commonalities(scanners: &[Scanner]) -> Vec<ScannerMatch> {
    scanners
        .into_iter()
        .enumerate()
        .tuple_combinations()
        .map(|((a_idx, a), (b_idx, b))| {
            a.all_arrangements()
                .into_iter()
                .cartesian_product(b.all_arrangements().into_iter())
                .find_map(|((arrange_a, a_beacons), (arrange_b, b_beacons))| {
                    a_beacons
                        .into_iter()
                        .cartesian_product(b_beacons.into_iter())
                        .into_group_map_by(|(a, b)| (*a - *b))
                        .into_iter()
                        .find_map(|(_, a_and_b_coords)| {
                            if a_and_b_coords.len() >= 12 {
                                Some(ScannerMatch {
                                    a_idx,
                                    a_arr: arrange_a,
                                    a_beacons: a_and_b_coords.iter().map(|(a, _)| *a).collect(),
                                    b_idx,
                                    b_arr: arrange_b,
                                    b_beacons: a_and_b_coords.iter().map(|(_, b)| *b).collect(),
                                })
                            } else {
                                None
                            }
                        })
                })
        })
        .flatten()
        .collect()
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

#[derive(Debug)]
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
