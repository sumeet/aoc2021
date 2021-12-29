#[derive(Copy, Clone)]
struct State {
    hallway: [Space; 11],
    rooms: [[Space; 2]; 4],
}

impl State {
    fn is_done(&self) -> bool {
        use Amphipod::*;
        self.rooms
            == [
                [Amphipod(A), Amphipod(A)],
                [Amphipod(B), Amphipod(B)],
                [Amphipod(C), Amphipod(C)],
                [Amphipod(D), Amphipod(D)],
            ]
    }
}

#[derive(Copy, Clone)]
enum Space {
    Empty,
    Amphipod(Amphipod),
}

#[derive(Copy, Clone)]
#[repr(usize)]
enum Amphipod {
    A,
    B,
    C,
    D,
}

// key is Amphipod
const ENERGY_PER_STEP: [usize; 4] = [1, 10, 100, 1000];

fn main() {
    println!("Hello, world!");
}
