use itertools::{iproduct, Itertools};
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
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

#[derive(Copy, Clone, Debug)]
struct ConversionInstructions {
    from_idx: usize,
    to_idx: usize,
    subtract_diff: Coord,
    reverse_arrangements: [Arrangement; 2],
}

impl ConversionInstructions {
    fn apply(&self, coord: Coord) -> Coord {
        (coord)
            .in_arrangement(self.reverse_arrangements[1])
            .reverse_arrangement(self.reverse_arrangements[0])
    }
}

type ConversionToFromMap = HashMap<usize, HashMap<usize, ConversionInstructions>>;

fn conversion_path(
    map: &ConversionToFromMap,
    from_idx: usize,
    to_idx: usize,
    depth: usize,
) -> Option<Vec<ConversionInstructions>> {
    if from_idx == to_idx {
        return None;
    }

    // hax
    if depth == 100 {
        return None;
    }

    if let Some(conversion_instructions) = map.get(&to_idx).and_then(|map| map.get(&from_idx)) {
        return Some(vec![conversion_instructions.clone()]);
    }

    for (inner_from_idx, inner_instructions) in map.get(&to_idx).unwrap().iter() {
        if let Some(mut path) = conversion_path(map, from_idx, *inner_from_idx, depth + 1) {
            path.push(*inner_instructions);
            return Some(path);
        }
    }
    None
}

// returned from target perspective
fn find_matches(target: &Scanner, cand: &Scanner) -> Option<Vec<Coord>> {
    target
        .all_arrangements()
        .into_iter()
        .cartesian_product(cand.all_arrangements().into_iter())
        .find_map(|((arrange_a, a_beacons), (_arrange_b, b_beacons))| {
            a_beacons
                .into_iter()
                .cartesian_product(b_beacons.into_iter())
                .into_group_map_by(|(a, b)| (*a - *b))
                .into_iter()
                .find_map(|(diff, a_and_b_coords)| {
                    if a_and_b_coords.len() >= 12 {
                        Some(
                            a_and_b_coords
                                .iter()
                                .map(|(_, b)| (diff + *b).reverse_arrangement(arrange_a))
                                .collect(),
                        )
                    } else {
                        None
                    }
                })
        })
}

fn main() {
    let parsed = parse(include_str!("../sample"));

    let mut coord_counts: HashMap<Coord, usize> = parsed[0]
        .beacons
        .iter()
        .cloned()
        .map(|coord| (coord, 1))
        .collect();

    //dbg!(&coord_counts);
    let mut q = parsed[1..].iter().cloned().collect_vec();
    while !q.is_empty() {
        let mut next_q = vec![];
        for next_scanner in &q {
            let this_scanner = Scanner {
                beacons: coord_counts.keys().cloned().collect(),
            };
            if let Some(matches) = find_matches(&this_scanner, &next_scanner) {
                dbg!("it's happening");
                dbg!(&matches);
                for coord in matches {
                    *coord_counts.entry(coord).or_insert(0) += 1;
                }
                continue; // without reinserting
            }
            // we'll try again next time
            next_q.push(next_scanner.clone());
        }
        q = next_q;
        dbg!(q.len());
    }
    dbg!(&coord_counts.values().filter(|&&count| count >= 2).count());
}

fn _main2() {
    let parsed = parse(include_str!("../sample"));
    let matches = find_commonalities(&parsed);
    // dbg!(&matches[0]);

    // let beacons_in_scanner_0_coordinates = matches
    //     .iter()
    //     .filter(|m| m.a_idx == 0)
    //     .map(|m| {
    //         m.a_beacons
    //             .iter()
    //             .map(move |beacon| beacon.reverse_arrangement(m.a_arr))
    //             .chain(
    //                 m.b_beacons
    //                     .iter()
    //                     .map(move |beacon| (*beacon + m.a_diff_b).reverse_arrangement(m.a_arr)),
    //             )
    //     })
    //     .flatten()
    //     .collect::<HashSet<_>>();

    let mut convert_map = ConversionToFromMap::new();

    for mach in &matches {
        // to scanner a from scanner b
        convert_map
            .entry(mach.a_idx)
            .or_insert(HashMap::new())
            .insert(
                mach.b_idx,
                ConversionInstructions {
                    from_idx: mach.b_idx,
                    to_idx: mach.a_idx,
                    subtract_diff: -mach.a_diff_b,
                    reverse_arrangements: [mach.a_arr, mach.b_arr],
                },
            );
        // to scanner b from scanner a
        convert_map
            .entry(mach.b_idx)
            .or_insert(HashMap::new())
            .insert(
                mach.a_idx,
                ConversionInstructions {
                    from_idx: mach.a_idx,
                    to_idx: mach.b_idx,
                    subtract_diff: mach.a_diff_b,
                    reverse_arrangements: [mach.b_arr, mach.a_arr],
                },
            );
    }

    // let mut found_beacons_by_scanner_idx = HashMap::new();
    //
    // let matches_by_a_idx = matches.iter().into_group_map_by(|m| m.a_idx);
    // for (a_idx, matches) in matches_by_a_idx {
    //     found_beacons_by_scanner_idx
    //         .entry(a_idx)
    //         .or_insert(HashSet::new())
    //         .extend(
    //             matches
    //                 .iter()
    //                 .map(|m| {
    //                     m.a_beacons
    //                         .iter()
    //                         .map(move |beacon| beacon.reverse_arrangement(m.a_arr))
    //                         .chain(m.b_beacons.iter().map(move |beacon| {
    //                             (*beacon + m.a_diff_b)
    //                                 .reverse_arrangement(m.a_arr)
    //                                 .reverse_arrangement(m.b_arr)
    //                         }))
    //                 })
    //                 .flatten(),
    //         );
    // }
    //
    // let matches_by_b_idx = matches.iter().into_group_map_by(|m| m.b_idx);
    // for (b_idx, matches) in matches_by_b_idx {
    //     found_beacons_by_scanner_idx
    //         .entry(b_idx)
    //         .or_insert(HashSet::new())
    //         .extend(
    //             matches
    //                 .iter()
    //                 .map(|m| {
    //                     m.b_beacons
    //                         .iter()
    //                         .map(move |beacon| beacon.reverse_arrangement(m.b_arr))
    //                         .chain(m.a_beacons.iter().map(move |beacon| {
    //                             (*beacon - m.a_diff_b)
    //                                 .reverse_arrangement(m.b_arr)
    //                                 .reverse_arrangement(m.a_arr)
    //                         }))
    //                 })
    //                 .flatten(),
    //         );
    // }

    // let mut scanner_0_beacons = HashSet::new();
    // for mach in matches {
    //     if mach.a_idx == 0 {
    //         scanner_0_beacons.extend(
    //             mach.a_beacons
    //                 .iter()
    //                 .map(|beacon| beacon.reverse_arrangement(mach.a_arr)),
    //         );
    //     } else {
    //         let path = conversion_path(&convert_map, mach.a_idx, 0, 0).unwrap();
    //         scanner_0_beacons.extend(mach.a_beacons.iter().copied().map(|mut beacon| {
    //             beacon = beacon.reverse_arrangement(mach.a_arr);
    //             for conv in &path {
    //                 beacon = conv.apply(beacon);
    //             }
    //             beacon
    //         }))
    //     }
    // }
    // dbg!(scanner_0_beacons);

    let path = conversion_path(&convert_map, 1, 0, 0).unwrap();
    dbg!(&path);
    let beacons = parsed[1].beacons.clone();
    let test = beacons.iter().cloned().map(|mut beacon| {
        for instruction in path.iter() {
            beacon = instruction.apply(beacon);
        }
        beacon
    });
    dbg!(test.collect_vec());

    // let mut scanner_0_beacons = HashSet::new();
    // scanner_0_beacons.extend(found_beacons_by_scanner_idx.remove(&0).unwrap());
    // dbg!(scanner_0_beacons.len());
    // //dbg!(found_beacons_by_scanner_idx.iter().map(|(idx, beacons)| (idx, beacons.len())).collect_vec());
    //
    // for (scanner_idx, beacons) in found_beacons_by_scanner_idx.into_iter() {
    //     let mut beacons = beacons.into_iter().collect_vec();
    //     let path = conversion_path(&convert_map, scanner_idx, 0, 0).unwrap();
    //     for instructions in path {
    //         beacons = beacons
    //             .into_iter()
    //             .map(|beacon| instructions.apply(beacon))
    //             .collect();
    //     }
    //     scanner_0_beacons.extend(beacons);
    // }
    //
    // dbg!(scanner_0_beacons.len());
}

#[derive(Debug, Clone)]
struct ScannerMatch {
    a_idx: usize,
    a_arr: Arrangement,
    a_beacons: Vec<Coord>,
    b_idx: usize,
    b_arr: Arrangement,
    b_beacons: Vec<Coord>,
    a_diff_b: Coord,
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
                        .find_map(|(diff, a_and_b_coords)| {
                            if a_and_b_coords.len() >= 12 {
                                Some(ScannerMatch {
                                    a_diff_b: diff,
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
