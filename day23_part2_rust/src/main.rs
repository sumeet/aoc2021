#![feature(iter_advance_by)]
use pathfinding::prelude::dijkstra;
use std::fmt::{Formatter, Write};
use std::str::FromStr;

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
    rooms: [[Space; 4]; 4],
}

impl From<char> for Space {
    fn from(c: char) -> Self {
        match c {
            '.' => Space::Empty,
            'A' => Space::Amphipod(Amphipod::A),
            'B' => Space::Amphipod(Amphipod::B),
            'C' => Space::Amphipod(Amphipod::C),
            'D' => Space::Amphipod(Amphipod::D),
            _ => panic!("Invalid character: {}", c),
        }
    }
}

impl FromStr for State {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        // skip the first line
        lines.next().unwrap();

        // the next line is the hallway
        let mut hallway_line = lines.next().unwrap().chars();
        // skip the "#"
        hallway_line.advance_by(1).unwrap();
        let mut hallway = [Space::Empty; 11];
        for (i, hallway_c) in hallway_line.take(HALLWAY_MAX + 1).enumerate() {
            hallway[i] = hallway_c.into();
        }

        // the next line is the top of the rooms
        let mut rooms = [[Space::Empty; 4]; 4];
        let top_room = lines.next().unwrap().chars();
        let bottom_room = lines.next().unwrap().chars();
        for (mut i, room) in [top_room, bottom_room].iter_mut().enumerate() {
            // special handling for part 2, the second space in the room is at the bottom
            i = if i == 0 { 0 } else { 3 };

            room.advance_by(3).unwrap();
            rooms[0][i] = room.next().unwrap().into();
            room.advance_by(1).unwrap();
            rooms[1][i] = room.next().unwrap().into();
            room.advance_by(1).unwrap();
            rooms[2][i] = room.next().unwrap().into();
            room.advance_by(1).unwrap();
            rooms[3][i] = room.next().unwrap().into();
        }

        // the middle room positions are hardcoded from the puzzle description
        rooms[0][1] = Space::Amphipod(Amphipod::D);
        rooms[0][2] = Space::Amphipod(Amphipod::D);
        rooms[1][1] = Space::Amphipod(Amphipod::C);
        rooms[1][2] = Space::Amphipod(Amphipod::B);
        rooms[2][1] = Space::Amphipod(Amphipod::B);
        rooms[2][2] = Space::Amphipod(Amphipod::A);
        rooms[3][1] = Space::Amphipod(Amphipod::A);
        rooms[3][2] = Space::Amphipod(Amphipod::C);

        Ok(Self { hallway, rooms })
    }
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

        for i in [1, 2, 3] {
            f.write_str("  #")?;
            self.rooms[0][i].fmt(f)?;
            f.write_char('#')?;
            self.rooms[1][i].fmt(f)?;
            f.write_char('#')?;
            self.rooms[2][i].fmt(f)?;
            f.write_char('#')?;
            self.rooms[3][i].fmt(f)?;
            f.write_str("#  \n")?;
        }

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

    fn amphipods_in_room(&self, room: usize) -> impl Iterator<Item = Amphipod> + '_ {
        self.rooms[room].iter().filter_map(|space| match space {
            Space::Amphipod(amphipod) => Some(*amphipod),
            _ => None,
        })
    }

    fn is_done(&self) -> bool {
        use crate::Amphipod::*;
        use Space::Amphipod;
        self.rooms
            == [
                [Amphipod(A), Amphipod(A), Amphipod(A), Amphipod(A)],
                [Amphipod(B), Amphipod(B), Amphipod(B), Amphipod(B)],
                [Amphipod(C), Amphipod(C), Amphipod(C), Amphipod(C)],
                [Amphipod(D), Amphipod(D), Amphipod(D), Amphipod(D)],
            ]
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
            [Empty, Empty, Empty, Empty] => (),
            [Amphipod(a), _, _, _] => {
                let entrance = ROOM_ENTRANCES[room_no];
                let going_left = Route::new(entrance, HALLWAY_MIN).0.enumerate();
                let going_right = Route::new(entrance, HALLWAY_MAX).0.enumerate();
                for route in [going_left, going_right] {
                    'inner: for (num_steps, hallway_pos) in route {
                        if state.hallway[hallway_pos] != Empty {
                            break 'inner;
                        }

                        // not allowed to stop in front of a room entrance
                        if !ROOM_ENTRANCES.contains(&hallway_pos) {
                            succs.push((
                                state
                                    .empty_room_pos(room_no, 0)
                                    .fill_hallway(hallway_pos, a),
                                // one step to get out into the hallway
                                // and enumerate is indexed by 0
                                energy_required(a, num_steps + 2),
                            ));
                        }
                    }
                }
            }
            [Empty, Amphipod(a), _, _] => {
                if a as usize == room_no && spaces[2] == Amphipod(a) && spaces[3] == Amphipod(a) {
                    // if we're already in the destination room, no point in moving
                    continue;
                }
                // just move to the other spot... if we want to move into the hallway, we can do it
                // once we're there using the above step
                succs.push((
                    state
                        .empty_room_pos(room_no, 1)
                        .fill_room_pos(room_no, 0, a),
                    energy_required(a, 1),
                ));
            }
            [_, Empty, Amphipod(a), _] => {
                if a as usize == room_no && spaces[3] == Amphipod(a) {
                    // if we're already in the destination room, no point in moving
                    continue;
                }
                // just move to the other spot... if we want to move into the hallway, we can do it
                // once we're there using the above step
                succs.push((
                    state
                        .empty_room_pos(room_no, 2)
                        .fill_room_pos(room_no, 1, a),
                    energy_required(a, 1),
                ));
            }
            [_, _, Empty, Amphipod(a)] => {
                if a as usize == room_no {
                    // if we're already in the destination room, no point in moving
                    continue;
                }
                // just move to the other spot... if we want to move into the hallway, we can do it
                // once we're there using the above step
                succs.push((
                    state
                        .empty_room_pos(room_no, 3)
                        .fill_room_pos(room_no, 2, a),
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
        let room_0_is_open = state.rooms[amphipod as usize][0] == Empty;
        let room_1_is_open = room_0_is_open && state.rooms[amphipod as usize][1] == Empty;
        let room_2_is_open = room_1_is_open && state.rooms[amphipod as usize][2] == Empty;
        let room_3_is_open = room_2_is_open && state.rooms[amphipod as usize][3] == Empty;
        if !room_0_is_open && !room_1_is_open && !room_2_is_open && !room_3_is_open {
            continue;
        }
        // can't go in the room if there's a non-destination amphipod already in the room
        if state
            .amphipods_in_room(amphipod as usize)
            .any(|a| a != amphipod)
        {
            continue;
        }

        let (route, num_steps) = Route::new(hallway_pos, ROOM_ENTRANCES[amphipod as usize]);
        if route
            .into_iter()
            .all(|pos| matches!(state.hallway[pos], Empty))
        {
            if room_0_is_open {
                succs.push((
                    state
                        .empty_hallway(hallway_pos)
                        .fill_room_pos(amphipod as usize, 0, amphipod),
                    energy_required(amphipod, num_steps + 1),
                ));
            }
            if room_1_is_open {
                succs.push((
                    state
                        .empty_hallway(hallway_pos)
                        .fill_room_pos(amphipod as usize, 1, amphipod),
                    energy_required(amphipod, num_steps + 2),
                ));
            }
            if room_2_is_open {
                succs.push((
                    state
                        .empty_hallway(hallway_pos)
                        .fill_room_pos(amphipod as usize, 2, amphipod),
                    energy_required(amphipod, num_steps + 3),
                ));
            }
            if room_3_is_open {
                succs.push((
                    state
                        .empty_hallway(hallway_pos)
                        .fill_room_pos(amphipod as usize, 3, amphipod),
                    energy_required(amphipod, num_steps + 4),
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
    let init = INPUT.parse().unwrap();
    let (path, cost) = dijkstra(&init, successors, |s| s.is_done()).unwrap();
    for state in path {
        println!("{}", state);
        println!();
    }
    dbg!(cost);
}

#[allow(unused)]
const INPUT: &str = r#"#############
#...........#
###B#B#D#D###
  #C#A#A#C#
  #########"#;

#[allow(unused)]
const SAMPLE: &str = r#"#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"#;
