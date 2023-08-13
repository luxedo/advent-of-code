/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/17
 *
 *  --- Day 17: Pyroclastic Flow ---
 * Your handheld device has located an alternative exit from the cave for you and the elephants. The ground is rumbling almost continuously now, but the strange valves bought you some time. It's definitely getting warmer in here, though.
 *
 * The tunnels eventually open into a very tall, narrow chamber. Large, oddly-shaped rocks are falling into the chamber from above, presumably due to all the rumbling. If you can't work out where the rocks will fall next, you might be crushed!
 *
 * The five types of rocks have the following peculiar shapes, where # is rock and . is empty space:
 *
 * ####
 *
 * .#.
 * ###
 * .#.
 *
 * ..#
 * ..#
 * ###
 *
 * #
 * #
 * #
 * #
 *
 * ##
 * ##
 * The rocks fall in the order shown above: first the - shape, then the + shape, and so on. Once the end of the list is reached, the same order repeats: the - shape falls first, sixth, 11th, 16th, etc.
 *
 * The rocks don't spin, but they do get pushed around by jets of hot gas coming out of the walls themselves. A quick scan reveals the effect the jets of hot gas will have on the rocks as they fall (your puzzle input).
 *
 * For example, suppose this was the jet pattern in your cave:
 *
 * >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
 * In jet patterns, < means a push to the left, while > means a push to the right. The pattern above means that the jets will push a falling rock right, then right, then right, then left, then left, then right, and so on. If the end of the list is reached, it repeats.
 *
 * The tall, vertical chamber is exactly seven units wide. Each rock appears so that its left edge is two units away from the left wall and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).
 *
 * After a rock appears, it alternates between being pushed by a jet of hot gas one unit (in the direction indicated by the next symbol in the jet pattern) and then falling one unit down. If any movement would cause any part of the rock to move into the walls, floor, or a stopped rock, the movement instead does not occur. If a downward movement would have caused a falling rock to move into the floor or an already-fallen rock, the falling rock stops where it is (having landed on something) and a new rock immediately begins falling.
 *
 * Drawing falling rocks with @ and stopped rocks with #, the jet pattern in the example above manifests as follows:
 *
 * The first rock begins falling:
 * |..@@@@.|
 * |.......|
 * |.......|
 * |.......|
 * +-------+
 *
 * Jet of gas pushes rock right:
 * |...@@@@|
 * |.......|
 * |.......|
 * |.......|
 * +-------+
 *
 * Rock falls 1 unit:
 * |...@@@@|
 * |.......|
 * |.......|
 * +-------+
 *
 * Jet of gas pushes rock right, but nothing happens:
 * |...@@@@|
 * |.......|
 * |.......|
 * +-------+
 *
 * Rock falls 1 unit:
 * |...@@@@|
 * |.......|
 * +-------+
 *
 * Jet of gas pushes rock right, but nothing happens:
 * |...@@@@|
 * |.......|
 * +-------+
 *
 * Rock falls 1 unit:
 * |...@@@@|
 * +-------+
 *
 * Jet of gas pushes rock left:
 * |..@@@@.|
 * +-------+
 *
 * Rock falls 1 unit, causing it to come to rest:
 * |..####.|
 * +-------+
 *
 * A new rock begins falling:
 * |...@...|
 * |..@@@..|
 * |...@...|
 * |.......|
 * |.......|
 * |.......|
 * |..####.|
 * +-------+
 *
 * Jet of gas pushes rock left:
 * |..@....|
 * |.@@@...|
 * |..@....|
 * |.......|
 * |.......|
 * |.......|
 * |..####.|
 * +-------+
 *
 * Rock falls 1 unit:
 * |..@....|
 * |.@@@...|
 * |..@....|
 * |.......|
 * |.......|
 * |..####.|
 * +-------+
 *
 * Jet of gas pushes rock right:
 * |...@...|
 * |..@@@..|
 * |...@...|
 * |.......|
 * |.......|
 * |..####.|
 * +-------+
 *
 * Rock falls 1 unit:
 * |...@...|
 * |..@@@..|
 * |...@...|
 * |.......|
 * |..####.|
 * +-------+
 *
 * Jet of gas pushes rock left:
 * |..@....|
 * |.@@@...|
 * |..@....|
 * |.......|
 * |..####.|
 * +-------+
 *
 * Rock falls 1 unit:
 * |..@....|
 * |.@@@...|
 * |..@....|
 * |..####.|
 * +-------+
 *
 * Jet of gas pushes rock right:
 * |...@...|
 * |..@@@..|
 * |...@...|
 * |..####.|
 * +-------+
 *
 * Rock falls 1 unit, causing it to come to rest:
 * |...#...|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * A new rock begins falling:
 * |....@..|
 * |....@..|
 * |..@@@..|
 * |.......|
 * |.......|
 * |.......|
 * |...#...|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 * The moment each of the next few rocks begins falling, you would see this:
 *
 * |..@....|
 * |..@....|
 * |..@....|
 * |..@....|
 * |.......|
 * |.......|
 * |.......|
 * |..#....|
 * |..#....|
 * |####...|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |..@@...|
 * |..@@...|
 * |.......|
 * |.......|
 * |.......|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |..@@@@.|
 * |.......|
 * |.......|
 * |.......|
 * |....##.|
 * |....##.|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |...@...|
 * |..@@@..|
 * |...@...|
 * |.......|
 * |.......|
 * |.......|
 * |.####..|
 * |....##.|
 * |....##.|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |....@..|
 * |....@..|
 * |..@@@..|
 * |.......|
 * |.......|
 * |.......|
 * |..#....|
 * |.###...|
 * |..#....|
 * |.####..|
 * |....##.|
 * |....##.|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |..@....|
 * |..@....|
 * |..@....|
 * |..@....|
 * |.......|
 * |.......|
 * |.......|
 * |.....#.|
 * |.....#.|
 * |..####.|
 * |.###...|
 * |..#....|
 * |.####..|
 * |....##.|
 * |....##.|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |..@@...|
 * |..@@...|
 * |.......|
 * |.......|
 * |.......|
 * |....#..|
 * |....#..|
 * |....##.|
 * |....##.|
 * |..####.|
 * |.###...|
 * |..#....|
 * |.####..|
 * |....##.|
 * |....##.|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 *
 * |..@@@@.|
 * |.......|
 * |.......|
 * |.......|
 * |....#..|
 * |....#..|
 * |....##.|
 * |##..##.|
 * |######.|
 * |.###...|
 * |..#....|
 * |.####..|
 * |....##.|
 * |....##.|
 * |....#..|
 * |..#.#..|
 * |..#.#..|
 * |#####..|
 * |..###..|
 * |...#...|
 * |..####.|
 * +-------+
 * To prove to the elephants your simulation is accurate, they want to know how tall the tower will get after 2022 rocks have stopped (but before the 2023rd rock begins falling). In this example, the tower of rocks will be 3068 units tall.
 *
 * How many units tall will the tower of rocks be after 2022 rocks have stopped falling?
 */

use aoc_rust::load_input;
use std::error::Error;
use std::fmt;
use std::str::FromStr;

const WIDTH: usize = 7;
const LEFT_BORDER: usize = 2;
const DROP_HEIGHT: usize = 3;
const ROCKS: &str = "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##";

#[derive(Clone, PartialEq)]
struct Coord {
    x: usize,
    y: usize,
}

#[derive(Clone)]
struct Rock {
    shape: Vec<Coord>,
    height: usize,
    width: usize,
}
impl fmt::Debug for Rock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rock_str = (0..self.height)
            .map(|y| {
                (0..self.width)
                    .map(move |x| {
                        if self.shape.iter().find(|c| **c == Coord { x, y }).is_some() {
                            '#'
                        } else {
                            ' '
                        }
                    })
                    .collect::<String>()
                    + "\n"
            })
            .collect::<String>();
        write!(f, "{}", rock_str)
    }
}
#[derive(Debug)]
struct ParseRockError;
impl FromStr for Rock {
    type Err = ParseRockError;
    fn from_str(shape: &str) -> Result<Self, Self::Err> {
        let shape = shape
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars()
                    .enumerate()
                    .filter_map(|(x, c)| if c == '#' { Some(Coord { x, y }) } else { None })
                    .collect::<Vec<Coord>>()
            })
            .collect::<Vec<Coord>>();
        let height = shape
            .iter()
            .map(|c| c.y + 1)
            .max()
            .expect("Please let me have some");
        let width = shape
            .iter()
            .map(|c| c.x + 1)
            .max()
            .expect("Please let me have some");
        Ok(Self {
            shape,
            height,
            width,
        })
    }
}

struct Rocks {
    rocks: Vec<Rock>,
    cursor: usize,
}
#[derive(Debug)]
struct ParseRocksError;
impl FromStr for Rocks {
    type Err = ParseRocksError;
    fn from_str(rocks: &str) -> Result<Self, Self::Err> {
        let rocks = rocks
            .split("\n\n")
            .map(|rock| rock.parse::<Rock>().expect("All is good with the rocks"))
            .collect::<Vec<Rock>>();
        Ok(Self { rocks, cursor: 0 })
    }
}
impl Rocks {
    fn next(&mut self) -> &Rock {
        let rock = &self.rocks[self.cursor];
        self.cursor = (self.cursor + 1) % self.rocks.len();
        rock
    }
}

struct RockFall {
    falling: Vec<bool>,
    stopped: Vec<bool>,
    rocks: Rocks,
    jet: Jet,
    width: usize,
    left_border: usize,
    drop_height: usize,
    height: usize,
}
impl fmt::Debug for RockFall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fall_str = (0..self.height)
            .map(|y| {
                let line = (0..self.width)
                    .map(|x| {
                        if self.get_falling(x, y) {
                            '@'
                        } else if self.get_stopped(x, y) {
                            '#'
                        } else {
                            '.'
                        }
                    })
                    .collect::<String>();
                format!("|{}|\n", line)
            })
            .collect::<String>();
        let bottom = format!("+{}+", (0..self.width).map(|_| '-').collect::<String>());
        write!(f, "\n{}{}", fall_str, bottom)
    }
}

impl RockFall {
    fn new(rocks: Rocks, jet: Jet, width: usize, left_border: usize, drop_height: usize) -> Self {
        let falling = vec![false; width * drop_height];
        let stopped = vec![false; width * drop_height];
        Self {
            stopped,
            falling,
            rocks,
            jet,
            width,
            left_border,
            drop_height,
            height: drop_height,
        }
    }
    fn get_stopped(&self, x: usize, y: usize) -> bool {
        self.stopped[x + y * self.width]
    }
    fn get_falling(&self, x: usize, y: usize) -> bool {
        self.falling[x + y * self.width]
    }
    fn set_falling(&mut self, x: usize, y: usize, value: bool) {
        self.falling[x + y * self.width] = value;
    }
    fn drop_next(&mut self) -> usize {
        let mut rock = self.rocks.next().clone();
        self.new_lines(rock.height); // TODO: Calculate how many new rows
        rock.shape.iter().for_each(|r| {
            self.set_falling(r.x + self.left_border, r.y, true);
        });
        loop {
            // push
            // fall
            // self.drop_falling(1);
        }
        dbg!(self.height);
        dbg!(self.jet.next());
        10
    }
    fn new_lines(&mut self, lines: usize) {
        self.height += lines;
        self.falling.extend(vec![false; lines * self.width]);
        self.stopped.extend(vec![false; lines * self.width]);
    }
}

#[derive(Debug)]
enum Side {
    Left,
    Right,
}
#[derive(Debug)]
struct Jet {
    pattern: Vec<Side>,
    cursor: usize,
}
#[derive(Debug)]
struct ParseJetError;
impl FromStr for Jet {
    type Err = ParseJetError;
    fn from_str(pattern: &str) -> Result<Self, Self::Err> {
        let pattern = pattern
            .chars()
            .map(|c| match c {
                '<' => Side::Left,
                '>' => Side::Right,
                _ => panic!("Malformed input"),
            })
            .collect::<Vec<Side>>();
        Ok(Self { pattern, cursor: 0 })
    }
}
impl Jet {
    fn next(&mut self) -> &Side {
        let side = &self.pattern[self.cursor];
        self.cursor = (self.cursor + 1) % self.pattern.len();
        side
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    const DROPPED_ROCKS: usize = 2022;
    let jet = input_text.parse::<Jet>().expect("Well formed input");
    let rocks = ROCKS.parse::<Rocks>().expect("All good with rocks");
    let mut rock_fall = RockFall::new(rocks, jet, WIDTH, LEFT_BORDER, DROP_HEIGHT);
    rock_fall.drop_next();
    dbg!(&rock_fall);
    0
}

fn solve_pt2(input_text: &str) -> u64 {
    1
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_17_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // Correct: AAA

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // Correct: BBB

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

    test_solution!(test1, solve_pt1, 3068, TEST_DATA);
    test_solution!(test2, solve_pt2, 1, TEST_DATA);
}
