/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 18
* Rust Solution
*
* Day 18: Boiling Boulders
*
* https://adventofcode.com/2022/day/18
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

#[derive(Clone)]
struct LavaStream {
    max_x: usize,
    max_y: usize,
    max_z: usize,
    boulders: Vec<u8>,
}

impl LavaStream {
    fn new(max_x: usize, max_y: usize, max_z: usize) -> Self {
        LavaStream {
            max_x,
            max_y,
            max_z,
            boulders: vec![0; (max_x + 2) * (max_y + 2) * (max_z + 2)],
        }
    }
    fn get_offset(stream: &Self, x: usize, y: usize, z: usize) -> usize {
        if x > stream.max_x + 2 || y > stream.max_y + 2 || z > stream.max_z + 2 {
            panic!("Can't calculate offest for boulder {x},{y},{z}");
        }
        return x + (stream.max_x + 2) * y + (stream.max_x + 2) * (stream.max_y + 2) * z;
    }
    fn set_boulder(&mut self, value: u8, x: usize, y: usize, z: usize) {
        let offest = Self::get_offset(self, x, y, z);
        self.boulders[offest] = value
    }
    fn get_boulder(&self, x: usize, y: usize, z: usize) -> u8 {
        self.boulders[Self::get_offset(self, x, y, z)]
    }
    fn from_sparse(lava_in: &Vec<(usize, usize, usize)>) -> Self {
        let max_x = lava_in.iter().max_by(|a, b| a.0.cmp(&b.0)).unwrap().0;
        let max_y = lava_in.iter().max_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
        let max_z = lava_in.iter().max_by(|a, b| a.2.cmp(&b.2)).unwrap().2;
        let mut lava_stream = LavaStream::new(max_x + 1, max_y + 1, max_z + 1);
        lava_in.iter().for_each(|(x, y, z)| {
            lava_stream.set_boulder(1, *x + 1, *y + 1, *z + 1); // Offset by one to allow searching
                                                                // bellow the 0th element. We're
                                                                // dointh this because we don't
                                                                // want to deal with isize
        });
        lava_stream
    }
    fn surface_area(stream: &Self) -> usize {
        let color = 0;
        (1..=(stream.max_z + 1))
            .flat_map(|z| {
                (1..=(stream.max_y + 1))
                    .flat_map(|y| {
                        (1..=(stream.max_x + 1))
                            .map(|x| Self::boulder_exposed_area(stream, x, y, z, color))
                            .collect::<Vec<usize>>()
                    })
                    .collect::<Vec<usize>>()
            })
            .sum::<usize>()
    }
    fn boulder_exposed_area(stream: &Self, x: usize, y: usize, z: usize, color: u8) -> usize {
        if stream.get_boulder(x, y, z) == color {
            return 0;
        }
        let sides = [
            (x.saturating_add(1), y, z),
            (x, y.saturating_add(1), z),
            (x, y, z.saturating_add(1)),
            (x.saturating_sub(1), y, z),
            (x, y.saturating_sub(1), z),
            (x, y, z.saturating_sub(1)),
        ];
        sides
            .map(|(xs, ys, zs)| {
                if stream.get_boulder(xs, ys, zs) == color {
                    1
                } else {
                    0
                }
            })
            .iter()
            .sum::<usize>()
    }
    fn outer_surface_area(stream: &Self) -> usize {
        let mut fill_stream = &mut stream.clone();
        const FILL_COLOR: u8 = std::u8::MAX;
        Self::boundary_fill(&mut fill_stream, 1, 1, 1, FILL_COLOR);
        fill_stream.boulders = fill_stream
            .boulders
            .iter()
            .map(|color| match color {
                0 => 1,
                &FILL_COLOR => 0,
                c => *c,
            })
            .collect();
        LavaStream::surface_area(&fill_stream)
    }
    fn boundary_fill(stream: &mut Self, x: usize, y: usize, z: usize, fill_color: u8) {
        let mut to_fill = vec![(x, y, z)];

        while let Some((x, y, z)) = to_fill.pop() {
            let offest = Self::get_offset(stream, x, y, z);
            stream.boulders[offest] = fill_color;
            let sides = vec![
                (x.saturating_add(1), y, z),
                (x, y.saturating_add(1), z),
                (x, y, z.saturating_add(1)),
                (x.saturating_sub(1), y, z),
                (x, y.saturating_sub(1), z),
                (x, y, z.saturating_sub(1)),
            ];
            let sclone = stream.clone();
            let sides = sides.into_iter().filter(|(x, y, z)| {
                (x == x.clamp(&0, &(sclone.max_x + 1)))
                    && (y == y.clamp(&0, &(sclone.max_y + 1)))
                    && (z == z.clamp(&0, &(sclone.max_z + 1)))
                    && {
                        let color = sclone.boulders[Self::get_offset(&sclone, *x, *y, *z)];
                        color == 0
                    }
            });
            to_fill.extend(sides);
        }
    }
}

fn parse_input(input_text: &str) -> LavaStream {
    let lava_in = input_text
        .trim()
        .lines()
        .map(|line| {
            let mut coords = line.split(",").map(|i| i.parse::<usize>().unwrap());
            (
                coords.next().unwrap(),
                coords.next().unwrap(),
                coords.next().unwrap(),
            )
        })
        .collect::<Vec<(usize, usize, usize)>>();
    LavaStream::from_sparse(&lava_in)
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let lava_stream = parse_input(&input_data);
    Ok(LavaStream::surface_area(&lava_stream))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let lava_stream = parse_input(&input_data);
    Ok(LavaStream::outer_surface_area(&lava_stream))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
