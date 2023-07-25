/* Advent Of Code 2016 - day 1
 * https://adventofcode.com/2016/day/1
 *
 *
 * --- Day 1: No Time for a Taxicab ---
 * Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.
 *
 * Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
 *
 * You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.
 *
 * The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.
 *
 * There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?
 *
 * For example:
 *
 * Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
 * R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
 * R5, L5, R5, R3 leaves you 12 blocks away.
 * How many blocks away is Easter Bunny HQ?
 *
 *
 * --- Part Two ---
 * Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.
 *
 * For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.
 *
 * How many blocks away is the first location you visit twice?
 */

use aoc_lang_rust::load_input;

use std::collections::BTreeSet;
use std::error::Error;
use std::str::FromStr;

enum Orientation {
    North,
    East,
    South,
    West,
}

impl Orientation {
    fn right(&self) -> Orientation {
        match self {
            Orientation::North => Orientation::East,
            Orientation::East => Orientation::South,
            Orientation::South => Orientation::West,
            Orientation::West => Orientation::North,
        }
    }
    fn left(&self) -> Orientation {
        match self {
            Orientation::North => Orientation::West,
            Orientation::East => Orientation::North,
            Orientation::South => Orientation::East,
            Orientation::West => Orientation::South,
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
struct Coord(i32, i32);

enum Side {
    Left,
    Right,
}

#[derive(Debug)]
struct ParseSideError;
impl FromStr for Side {
    type Err = ParseSideError;
    fn from_str(input: &str) -> Result<Side, Self::Err> {
        match input {
            "L" => Ok(Side::Left),
            "R" => Ok(Side::Right),
            _ => Err(ParseSideError),
        }
    }
}

struct Taxi {
    orientation: Orientation,
    position: Coord,
}

impl Taxi {
    fn new() -> Taxi {
        Taxi {
            orientation: Orientation::North,
            position: Coord(0, 0),
        }
    }

    fn turn(&mut self, side: Side) {
        self.orientation = match side {
            Side::Left => self.orientation.left(),
            Side::Right => self.orientation.right(),
        };
    }
    fn step(&mut self, steps: u32) {
        for _ in 0..steps {
            match self.orientation {
                Orientation::North => self.position.0 += 1,
                Orientation::East => self.position.1 += 1,
                Orientation::South => self.position.0 -= 1,
                Orientation::West => self.position.1 -= 1,
            };
        }
    }
    fn manhattan(&self) -> u64 {
        self.position.0.abs().saturating_add(self.position.1.abs()) as u64
    }
}

fn parse_input(input_data: &str) -> Vec<(Side, u32)> {
    input_data
        .trim()
        .split(", ")
        .map(|command| command.split_at(1))
        .map(|(side, steps)| (Side::from_str(side).unwrap(), steps.parse::<u32>().unwrap()))
        .collect()
}

fn solve_pt1(input_data: &str) -> u64 {
    let taxi = Taxi::new();
    parse_input(input_data)
        .into_iter()
        .fold(taxi, |mut acc, (side, steps)| {
            acc.turn(side);
            acc.step(steps);
            acc
        })
        .manhattan()
}

fn solve_pt2(input_data: &str) -> u64 {
    let mut taxi = Taxi::new();
    let mut seen: BTreeSet<Coord> = BTreeSet::new();
    seen.insert(taxi.position);
    let _: Vec<_> = parse_input(input_data)
        .into_iter()
        .map_while(|(side, steps)| {
            taxi.turn(side);
            for _ in 0..steps {
                taxi.step(1);
                if seen.contains(&taxi.position) {
                    return None;
                }
                seen.insert(taxi.position);
            }
            Some(())
        })
        .collect();
    taxi.manhattan()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_01_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // Correct: 300

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // Correct: 159

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_lang_rust::test_solution;

    test_solution!(test1, solve_pt1, 5, "R2, L3");
    test_solution!(test2, solve_pt1, 2, "R2, R2, R2");
    test_solution!(test3, solve_pt1, 12, "R5, L5, R5, R3");
    test_solution!(test4, solve_pt2, 4, "R8, R4, R4, R8");
}
