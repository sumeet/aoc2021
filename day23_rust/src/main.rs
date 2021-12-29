// sample:
// #############
// #...........#
// ###B#C#B#D###
//   #A#D#C#A#
//   #########

use pathfinding::prelude::dijkstra;
use std::fmt::{Formatter, Write};

const ENERGY_PER_STEP: [usize; 4] = [1, 10, 100, 1000];
const ROOM_ENTRANCES: [usize; 4] = [2, 4, 6, 8];
const HALLWAY_MIN: usize = 0;
const HALLWAY_MAX: usize = 10;

fn energy_required(amphipod: Amphipod, num_steps: usize) -> usize {
    ENERGY_PER_STEP[amphipod as usize] * num_steps
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
struct State {
    hallway: [Space; 11],
    rooms: [[Space; 2]; 4],
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str("#############\n")?;

        f.write_char('#')?;
        for space in self.hallway {
            space.fmt(f)?;
        }
        f.write_char('#')?;
        f.write_char('\n')?;

        f.write_str("###")?;
        self.rooms[0][0].fmt(f)?;
        f.write_char('#')?;
        self.rooms[1][0].fmt(f)?;
        f.write_char('#')?;
        self.rooms[2][0].fmt(f)?;
        f.write_char('#')?;
        self.rooms[3][0].fmt(f)?;
        f.write_str("###\n")?;

        f.write_str("  #")?;
        self.rooms[0][1].fmt(f)?;
        f.write_char('#')?;
        self.rooms[1][1].fmt(f)?;
        f.write_char('#')?;
        self.rooms[2][1].fmt(f)?;
        f.write_char('#')?;
        self.rooms[3][1].fmt(f)?;
        f.write_str("#  \n")?;

        f.write_str("  #########  ")?;
        Ok(())
    }
}

impl State {
    fn fill_hallway(mut self, pos: usize, amphipod: Amphipod) -> Self {
        assert_eq!(self.hallway[pos], Space::Empty);
        self.hallway[pos] = Space::Amphipod(amphipod);
        self
    }

    fn empty_hallway(mut self, pos: usize) -> Self {
        assert_ne!(self.hallway[pos], Space::Empty);
        self.hallway[pos] = Space::Empty;
        self
    }

    fn fill_room_pos(mut self, room: usize, room_pos: usize, amphipod: Amphipod) -> Self {
        assert_eq!(self.rooms[room][room_pos], Space::Empty);
        self.rooms[room][room_pos] = Space::Amphipod(amphipod);
        self
    }

    fn empty_room_pos(mut self, room: usize, room_pos: usize) -> Self {
        assert_ne!(self.rooms[room][room_pos], Space::Empty);
        self.rooms[room][room_pos] = Space::Empty;
        self
    }

    fn is_done(&self) -> bool {
        use crate::Amphipod::*;
        use Space::Amphipod;
        self.rooms
            == [
                [Amphipod(A), Amphipod(A)],
                [Amphipod(B), Amphipod(B)],
                [Amphipod(C), Amphipod(C)],
                [Amphipod(D), Amphipod(D)],
            ]
    }

    fn sample() -> Self {
        use crate::Amphipod::*;
        use Space::*;
        Self {
            hallway: [Empty; 11],
            rooms: [
                [Amphipod(B), Amphipod(A)],
                [Amphipod(C), Amphipod(D)],
                [Amphipod(B), Amphipod(C)],
                [Amphipod(D), Amphipod(A)],
            ],
        }
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
enum Space {
    Empty,
    Amphipod(Amphipod),
}

impl std::fmt::Display for Space {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Space::Empty => f.write_char('.')?,
            Space::Amphipod(amphipod) => amphipod.fmt(f)?,
        }
        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
#[repr(usize)]
enum Amphipod {
    A,
    B,
    C,
    D,
}

impl std::fmt::Display for Amphipod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::Amphipod::*;
        f.write_char(match self {
            A => 'A',
            B => 'B',
            C => 'C',
            D => 'D',
        })
    }
}

fn successors(state: &State) -> Vec<(State, usize)> {
    use Space::*;
    let mut succs = vec![];
    hallway_succs(state, &mut succs);

    for (room_no, spaces) in state.rooms.iter().copied().enumerate() {
        match spaces {
            [Empty, Empty] => (),
            [Amphipod(a), _] => {
                // one step to get out into the hallway
                let mut num_steps = 1;
                let entrance = ROOM_ENTRANCES[room_no];
                for hallway_pos in Route::new(entrance, HALLWAY_MIN).0 {
                    num_steps += 1;

                    if state.hallway[hallway_pos] != Empty {
                        break;
                    }

                    // not allowed to stop in front of a room entrance
                    if !ROOM_ENTRANCES.contains(&hallway_pos) {
                        succs.push((
                            state
                                .empty_room_pos(room_no, 0)
                                .fill_hallway(hallway_pos, a),
                            energy_required(a, num_steps),
                        ));
                    }
                }
                num_steps = 1;
                for hallway_pos in Route::new(entrance, HALLWAY_MAX).0 {
                    num_steps += 1;

                    if state.hallway[hallway_pos] != Empty {
                        break;
                    }

                    // not allowed to stop in front of a room entrance
                    if !ROOM_ENTRANCES.contains(&hallway_pos) {
                        succs.push((
                            state
                                .empty_room_pos(room_no, 0)
                                .fill_hallway(hallway_pos, a),
                            energy_required(a, num_steps),
                        ));
                    }
                }
            }
            [Empty, Amphipod(a)] => {
                // just move to the other spot... if we want to move into the hallway, we can do it
                // once we're there using the above step
                succs.push((
                    state
                        .empty_room_pos(room_no, 1)
                        .fill_room_pos(room_no, 0, a),
                    energy_required(a, 1),
                ));
            }
        }
    }

    succs
}

fn hallway_succs(state: &State, succs: &mut Vec<(State, usize)>) {
    use Space::*;
    for (hallway_pos, amphipod) in
        state
            .hallway
            .iter()
            .copied()
            .enumerate()
            .filter_map(|(pos, space)| {
                if let Amphipod(amphipod) = space {
                    Some((pos, amphipod))
                } else {
                    None
                }
            })
    {
        let room_higher_is_open = state.rooms[amphipod as usize][0] == Empty;
        let room_lower_is_open = room_higher_is_open && state.rooms[amphipod as usize][1] == Empty;
        if !room_higher_is_open && !room_lower_is_open {
            continue;
        }
        let (route, num_steps) = Route::new(hallway_pos, ROOM_ENTRANCES[amphipod as usize]);
        if route
            .into_iter()
            .all(|pos| matches!(state.hallway[pos], Empty))
        {
            if room_higher_is_open {
                succs.push((
                    state
                        .empty_hallway(hallway_pos)
                        .fill_room_pos(amphipod as usize, 0, amphipod),
                    energy_required(amphipod, num_steps + 1),
                ));
            }
            if room_lower_is_open {
                succs.push((
                    state
                        .empty_hallway(hallway_pos)
                        .fill_room_pos(amphipod as usize, 0, amphipod),
                    energy_required(amphipod, num_steps + 2),
                ));
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Route(usize, usize);

impl Route {
    fn new(mut from: usize, to: usize) -> (Self, usize) {
        let num_steps = if from < to {
            from += 1;
            to - from + 1
        } else {
            from -= 1;
            from - to + 1
        };
        (Self(from, to), num_steps)
    }
}

impl Iterator for Route {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let Route(start, dest) = *self;

        if start == usize::MAX && dest == usize::MAX {
            return None;
        } else if start == usize::MAX {
            self.1 = usize::MAX;
            return Some(dest);
        }

        // iterate either upwards or downwards depending on if start is greater or less than dest
        if start < dest {
            let next = self.0;
            self.0 += 1;
            if self.0 == dest {
                self.0 = usize::MAX;
            }
            Some(next)
        } else if start > dest {
            let next = self.0;
            self.0 -= 1;
            if self.0 == dest {
                self.0 = usize::MAX;
            }
            Some(next)
        } else {
            let next = self.0;
            self.0 = usize::MAX;
            self.1 = usize::MAX;
            Some(next)
        }
    }
}

fn main() {
    let init = State::sample();
    println!("init:");
    println!("{}", init);
    println!();
    for (succ, _) in successors(&init) {
        println!("{}", succ);

        for (succ_succ, _) in successors(&succ) {
            let next_succ = format!("{}", succ_succ);
            for line in next_succ.lines() {
                println!("    {}", line);
            }

            for (succ_succ_succ, _) in successors(&succ_succ) {
                let next_succ = format!("{}", succ_succ_succ);
                for line in next_succ.lines() {
                    println!("        {}", line);
                }
            }
        }
    }
    // dbg!(dijkstra(&init, successors, |s| s.is_done()));
}
