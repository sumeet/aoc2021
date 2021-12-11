use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use rayon::ThreadPoolBuilder;

fn main() {
    //part1();
    part2();
}

fn part1() {
    let mut fishes = INPUT.split(",").map(|x| x.parse().unwrap()).collect::<Vec<u8>>();
    for i in 0..80 {
        let mut next_fishes = vec![];
        for fish in fishes.iter_mut() {
            if *fish == 0 {
                *fish = 6;
                next_fishes.push(8);
            } else {
                *fish -= 1;
            }
        }
        fishes.extend(next_fishes);
    }
    dbg!(fishes.len());
}

fn part2() {
    ThreadPoolBuilder::new().num_threads(4).build_global().unwrap();
    let mut cache = (0u8..8).into_par_iter().map(|fish| {
        let mut fishes = vec![fish];
        for _ in 0..256 {
            let mut next_fishes = vec![];
            for fish in fishes.iter_mut() {
                if *fish == 0 {
                    *fish = 6;
                    next_fishes.push(8);
                } else {
                    *fish -= 1;
                }
            }
            fishes.extend(next_fishes);
        }
        fishes.len() as _
    }).collect::<Vec<u128>>();

    dbg!(INPUT.split(",").map(|x| x.parse::<usize>().unwrap()).map(|n| cache[n]).sum::<u128>());

}

const SAMPLE : &str = "3,4,3,1,2";
const INPUT : &str = "3,4,3,1,2,1,5,1,1,1,1,4,1,2,1,1,2,1,1,1,3,4,4,4,1,3,2,1,3,4,1,1,3,4,2,5,5,3,3,3,5,1,4,1,2,3,1,1,1,4,1,4,1,5,3,3,1,4,1,5,1,2,2,1,1,5,5,2,5,1,1,1,1,3,1,4,1,1,1,4,1,1,1,5,2,3,5,3,4,1,1,1,1,1,2,2,1,1,1,1,1,1,5,5,1,3,3,1,2,1,3,1,5,1,1,4,1,1,2,4,1,5,1,1,3,3,3,4,2,4,1,1,5,1,1,1,1,4,4,1,1,1,3,1,1,2,1,3,1,1,1,1,5,3,3,2,2,1,4,3,3,2,1,3,3,1,2,5,1,3,5,2,2,1,1,1,1,5,1,2,1,1,3,5,4,2,3,1,1,1,4,1,3,2,1,5,4,5,1,4,5,1,3,3,5,1,2,1,1,3,3,1,5,3,1,1,1,3,2,5,5,1,1,4,2,1,2,1,1,5,5,1,4,1,1,3,1,5,2,5,3,1,5,2,2,1,1,5,1,5,1,2,1,3,1,1,1,2,3,2,1,4,1,1,1,1,5,4,1,4,5,1,4,3,4,1,1,1,1,2,5,4,1,1,3,1,2,1,1,2,1,1,1,2,1,1,1,1,1,4";