/* Advent Of Code 2016
 * https://adventofcode.com/2016/day/3
 *
 * --- Day 3: Squares With Three Sides ---
 * Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for triangles.
 *
 * Or are they?
 *
 * The design document gives the side lengths of each triangle it describes, but... 5 10 25?  Some of these aren't triangles. You can't help but mark the impossible ones.
 *
 * In a valid triangle, the sum of any two sides must be larger than the remaining side.  For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.
 *
 * In your puzzle input, how many of the listed triangles are possible?
*/
#![allow(dead_code)]

use aoc2016::load_input;
use std::error::Error;

struct Triangle {
    side_a: u32,
    side_b: u32,
    side_c: u32,
}

#[derive(Debug, Clone)]
struct TriangleError;
impl Triangle {
    fn new(side_a: u32, side_b: u32, side_c: u32) -> Result<Self, TriangleError> {
        let (side_a, side_b, side_c) = Self::sort3((side_a, side_b, side_c));
        if side_a + side_b > side_c {
            Ok(Self {
                side_a,
                side_b,
                side_c,
            })
        } else {
            Err(TriangleError)
        }
    }

    fn sort3(sides: (u32, u32, u32)) -> (u32, u32, u32) {
        let (a, b, c) = sides;
        let mut v = [a, b, c];
        v.sort();
        let [a, b, c] = v;
        (a, b, c)
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    input_text
        .lines()
        .map(|line| {
            let mut line = line
                .split_whitespace()
                .map(|item| item.parse::<u32>().unwrap());
            (
                line.next().unwrap(),
                line.next().unwrap(),
                line.next().unwrap(),
            )
        })
        .filter(|(side_a, side_b, side_c)| Triangle::new(*side_a, *side_b, *side_c).is_ok())
        .count()
        .try_into()
        .unwrap()
}

fn solve_pt2(input_text: &str) -> u64 {
    input_text
        .lines()
        .map(|line| {
            let mut line = line
                .split_whitespace()
                .map(|item| item.parse::<u32>().unwrap());
            (
                line.next().unwrap(),
                line.next().unwrap(),
                line.next().unwrap(),
            )
        })
        .collect::<Vec<_>>()
        .chunks(3)
        .map(|chunk| {
            Triangle::new(chunk[0].0, chunk[1].0, chunk[2].0).is_ok() as u64
                + Triangle::new(chunk[0].1, chunk[1].1, chunk[2].1).is_ok() as u64
                + Triangle::new(chunk[0].2, chunk[1].2, chunk[2].2).is_ok() as u64
        })
        .sum()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_03_input.txt";
    let input_text = load_input(FILENAME);

    print!("Part one: {:#?}\n", solve_pt1(&input_text));
    // solution_pt1: 993

    print!("Part two: {:#?}\n", solve_pt2(&input_text));
    // solution_pt2: 1849

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc2016::test_solution;

    test_solution!(test1, solve_pt1, 0, "5 10 25");
    test_solution!(
        test4,
        solve_pt2,
        6,
        "101 301 501\n\
         102 302 502\n\
         103 303 503\n\
         201 401 601\n\
         202 402 602\n\
         203 403 603"
    );
}
