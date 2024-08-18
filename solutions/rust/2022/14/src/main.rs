/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 14
* Rust Solution
*
* Day 14: Regolith Reservoir
*
* https://adventofcode.com/2022/day/14
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
use std::fmt;

#[derive(Debug)]
struct Coord {
    x: usize,
    y: usize,
}

struct Board {
    x_min: usize,
    x_max: usize,
    width: usize,
    height: usize,
    walls: Vec<bool>,
    sand: Vec<bool>,
}
impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let board_str = (0..self.height)
            .map(|y| {
                (self.x_min - 5..=self.x_max + 8)
                    .map(|x| match (self.get_wall(x, y), self.get_sand(x, y)) {
                        (Ok(true), Ok(_)) => '#',
                        (Ok(_), Ok(true)) => 'o',
                        _ => '.',
                    })
                    .collect::<String>()
                    + "\n"
            })
            .collect::<String>();
        write!(f, "\n{}", board_str)
    }
}
impl Board {
    fn new(width: usize, height: usize, x_min: usize, x_max: usize) -> Self {
        let length = width * height;
        let walls = vec![false; length];
        let sand = vec![false; length];
        let mut board = Board {
            x_min,
            x_max,
            width,
            height,
            walls,
            sand,
        };
        (0..width).for_each(|x| {
            board
                .set_wall(true, x, height - 1)
                .expect("Placing bottom wall");
        });
        board
    }
    fn from_str(input_text: &str) -> Self {
        let wall_coords = input_text
            .lines()
            .map(|line| {
                line.split(" -> ")
                    .map(|item| {
                        let (x, y) = item.split_once(",").unwrap();
                        let (x, y) = (x.parse::<usize>().unwrap(), y.parse::<usize>().unwrap());
                        Coord { x, y }
                    })
                    .collect::<Vec<Coord>>()
            })
            .collect::<Vec<Vec<Coord>>>();
        let x_min = wall_coords
            .iter()
            .map(|line| line.iter().map(|c| c.x).min().unwrap())
            .min()
            .unwrap();
        let x_max = wall_coords
            .iter()
            .map(|line| line.iter().map(|c| c.x).max().unwrap())
            .max()
            .unwrap();
        let y_max = wall_coords
            .iter()
            .map(|line| line.iter().map(|c| c.y).max().unwrap())
            .max()
            .unwrap();
        let mut board = Self::new(x_min + x_max, y_max + 3, x_min, x_max);
        wall_coords.iter().for_each(|line| {
            line.iter()
                .zip(line[1..].iter())
                .flat_map(|(prev, cur)| {
                    let dx = prev.x.abs_diff(cur.x);
                    let dy = prev.y.abs_diff(cur.y);
                    match (dx, dy) {
                        (0, dy) if dy > 0 => {
                            let x = prev.x;
                            let y_min = if prev.y < cur.y { prev.y } else { cur.y };
                            let y_max = if prev.y > cur.y { prev.y } else { cur.y };
                            (y_min..=y_max)
                                .map(|y| Coord { x, y })
                                .collect::<Vec<Coord>>()
                        }
                        (dx, 0) if dx > 0 => {
                            let y = prev.y;
                            let x_min = if prev.x < cur.x { prev.x } else { cur.x };
                            let x_max = if prev.x > cur.x { prev.x } else { cur.x };
                            (x_min..=x_max)
                                .map(|x| Coord { x, y })
                                .collect::<Vec<Coord>>()
                        }
                        _ => panic!("Oh no! No can diagonal"),
                    }
                })
                .for_each(|c| {
                    board.set_wall(true, c.x, c.y).expect("Wall is ok");
                });
        });
        board
    }

    fn get_offset(&self, x: usize, y: usize) -> Result<usize, &str> {
        if (x >= self.width) || (y > self.height) {
            return Err("Point {x}, {y} out of bounds");
        }
        let offset = x + y * self.width;
        Ok(offset)
    }
    fn set_wall(&mut self, value: bool, x: usize, y: usize) -> Result<(), &str> {
        let offset = self.get_offset(x, y);
        if offset.is_ok() {
            let offset = offset.unwrap();
            self.walls[offset] = value;
            return Ok(());
        } else {
            return Err("Cannot place wall");
        }
    }
    fn get_wall(&self, x: usize, y: usize) -> Result<bool, &str> {
        self.get_offset(x, y)
            .and_then(|offset| Ok(self.walls[offset]))
    }
    fn set_sand(&mut self, value: bool, x: usize, y: usize) -> Result<(), &str> {
        let offset = self.get_offset(x, y);
        if offset.is_ok() {
            let offset = offset.unwrap();
            self.sand[offset] = value;
            return Ok(());
        } else {
            return Err("Cannot place sand");
        }
    }
    fn get_sand(&self, x: usize, y: usize) -> Result<bool, &str> {
        self.get_offset(x, y)
            .and_then(|offset| Ok(self.sand[offset]))
    }
    fn drop_sand(&mut self, x: usize, y: usize) -> Result<Coord, &str> {
        //let mut sand_pos = Coord { x, y };
        let mut x = x;
        let mut y = y;
        let sand_pos = loop {
            // 1. Check bellow
            let wall_below = self.get_wall(x, y + 1);
            let sand_below = self.get_sand(x, y + 1);
            match (wall_below, sand_below) {
                (Ok(false), Ok(false)) => {
                    y += 1;
                    continue;
                }
                (Ok(_), Ok(_)) => (),
                (Err(_), _) => return Err("Falling out of bounds"),
                _ => (),
            };
            // 2. Check lower left diagonal
            let wall_b_left = self.get_wall(x - 1, y + 1);
            let sand_b_left = self.get_sand(x - 1, y + 1);
            match (wall_b_left, sand_b_left) {
                (Ok(false), Ok(false)) => {
                    x -= 1;
                    y += 1;
                    continue;
                }
                _ => (),
            };
            // 3. Check lower right diagonal
            let wall_b_right = self.get_wall(x + 1, y + 1);
            let sand_b_right = self.get_sand(x + 1, y + 1);
            match (wall_b_right, sand_b_right) {
                (Ok(false), Ok(false)) => {
                    x += 1;
                    y += 1;
                    continue;
                }
                _ => (),
            };
            // 4. Otherwise stop
            break Coord { x, y };
        };
        self.set_sand(true, sand_pos.x, sand_pos.y)
            .and_then(|()| Ok(sand_pos))
    }

    fn off_bounds(&self, x: usize, y: usize) -> bool {
        (x < self.x_min) || (x > self.x_max) || (y == self.height - 2)
    }

    fn drop_sand_until_fall(&mut self, x: usize, y: usize) -> usize {
        let mut i = 0;
        loop {
            let sand = self.drop_sand(x, y).expect("Sand dropped");
            if self.off_bounds(sand.x, sand.y) {
                return i;
            }
            i += 1;
        }
    }

    fn drop_sand_until_full(&mut self, x: usize, y: usize) -> usize {
        let mut i = 0;
        loop {
            let sand = self.drop_sand(x, y).expect("Sand dropped");
            if sand.x == x && sand.y == y {
                return i + 1;
            }
            i += 1;
        }
    }
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let mut board = Board::from_str(input_data);
    const SAND_X: usize = 500;
    const SAND_Y: usize = 0;
    Ok(board.drop_sand_until_fall(SAND_X, SAND_Y))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let mut board = Board::from_str(input_data);
    const SAND_X: usize = 500;
    const SAND_Y: usize = 0;
    Ok(board.drop_sand_until_full(SAND_X, SAND_Y))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
