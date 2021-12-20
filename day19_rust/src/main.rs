use itertools::Itertools;
use lazy_static::lazy_static;

lazy_static! {
    static ref ARRANGEMENTS: Vec<(Arrangement)> = {
        Dim::all_combos()
            .into_iter()
            .cartesian_product(all_flips().into_iter())
            .map(|(dims, flips)| Arrangement { dims, flips })
            .collect()
    };
}

fn main() {
    let parsed = parse();
    let to_rotate = &parsed[0];

    dbg!(to_rotate.all_rotations().len());
    dbg!(to_rotate.all_rotations());
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

#[derive(Copy, Clone, Debug)]
struct Coord {
    x: isize,
    y: isize,
    z: isize,
}

impl Coord {
    fn new(x: isize, y: isize, z: isize) -> Coord {
        Coord { x, y, z }
    }

    fn in_arrangement(self, Arrangement { dims, flips }: Arrangement) -> Self {
        let (x, y, z) = dims
            .iter()
            .zip(flips.iter())
            .map(|(dim, flip)| match dim {
                Dim::X => self.x,
                Dim::Y => self.y,
                Dim::Z => self.z,
            } * flip)
            .collect_tuple()
            .unwrap();
        Self { x, y, z }
    }
}

#[derive(Debug)]
struct Scanner {
    beacon_locs: Vec<Coord>,
}

impl Scanner {
    fn new() -> Self {
        Scanner {
            beacon_locs: Vec::new(),
        }
    }

    fn all_rotations(&self) -> Vec<Vec<Coord>> {
        ARRANGEMENTS
            .iter()
            .map(|arrangement| {
                self.beacon_locs
                    .iter()
                    .map(|loc| loc.in_arrangement(*arrangement))
                    .collect()
            })
            .collect()
    }
}

fn parse() -> Vec<Scanner> {
    let mut ret = vec![];
    let mut lines = include_str!("../rotsample").lines();
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
        this_scanner.beacon_locs.push(Coord::new(x, y, z));
    }
    ret.push(this_scanner);
    ret
}
