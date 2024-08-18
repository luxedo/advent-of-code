/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 17
* Rust Solution
*
* Day 17: Pyroclastic Flow
*
* https://adventofcode.com/2022/day/17
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
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
fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const DROPPED_ROCKS: usize = 2022;
    let jet = input_data.parse::<Jet>().expect("Well formed input");
    let rocks = ROCKS.parse::<Rocks>().expect("All good with rocks");
    let mut rock_fall = RockFall::new(rocks, jet, WIDTH, LEFT_BORDER, DROP_HEIGHT);
    rock_fall.drop_next();
    dbg!(&rock_fall);
    Ok(1)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    Ok("December")
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
