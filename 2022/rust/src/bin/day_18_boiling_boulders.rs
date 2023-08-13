/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/18
 *
 * --- Day 18: Boiling Boulders ---
 * You and the elephants finally reach fresh air. You've emerged near the base of a large volcano that seems to be actively erupting! Fortunately, the lava seems to be flowing away from you and toward the ocean.
 *
 * Bits of lava are still being ejected toward you, so you're sheltering in the cavern exit a little longer. Outside the cave, you can see the lava landing in a pond and hear it loudly hissing as it solidifies.
 *
 * Depending on the specific compounds in the lava and speed at which it cools, it might be forming obsidian! The cooling rate should be based on the surface area of the lava droplets, so you take a quick scan of a droplet as it flies past you (your puzzle input).
 *
 * Because of how quickly the lava is moving, the scan isn't very good; its resolution is quite low and, as a result, it approximates the shape of the lava droplet with 1x1x1 cubes on a 3D grid, each given as its x,y,z position.
 *
 * To approximate the surface area, count the number of sides of each cube that are not immediately connected to another cube. So, if your scan were only two adjacent cubes like 1,1,1 and 2,1,1, each cube would have a single side covered and five sides exposed, a total surface area of 10 sides.
 *
 * Here's a larger example:
 *
 * 2,2,2
 * 1,2,2
 * 3,2,2
 * 2,1,2
 * 2,3,2
 * 2,2,1
 * 2,2,3
 * 2,2,4
 * 2,2,6
 * 1,2,5
 * 3,2,5
 * 2,1,5
 * 2,3,5
 * In the above example, after counting up all the sides that aren't connected to another cube, the total surface area is 64.
 *
 * What is the surface area of your scanned lava droplet?
 *
 *
 * --- Part Two ---
 * Something seems off about your calculation. The cooling rate depends on exterior surface area, but your calculation also included the surface area of air pockets trapped in the lava droplet.
 *
 * Instead, consider only cube sides that could be reached by the water and steam as the lava droplet tumbles into the pond. The steam will expand to reach as much as possible, completely displacing any air on the outside of the lava droplet but never expanding diagonally.
 *
 * In the larger example above, exactly one cube of air is trapped within the lava droplet (at 2,2,5), so the exterior surface area of the lava droplet is 58.
 *
 * What is the exterior surface area of your scanned lava droplet?
 */

use aoc_rust::load_input;
use std::error::Error;

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

fn solve_pt1(input_text: &str) -> u64 {
    let lava_stream = parse_input(&input_text);
    LavaStream::surface_area(&lava_stream) as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    let lava_stream = parse_input(&input_text);
    LavaStream::outer_surface_area(&lava_stream) as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_18_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // Correct: 4282

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // Correct: 2452

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA_1: &str = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";
    const TEST_DATA_2: &str = "1,1,1
2,1,1";
    const TEST_DATA_3: &str = "1,1,1
2,1,1
3,1,1
4,1,1
5,1,1
6,1,1
1,2,1
2,2,1
3,2,1
4,2,1
5,2,1
6,2,1
1,3,1
2,3,1
3,3,1
4,3,1
5,3,1
6,3,1
1,1,2
2,1,2
3,1,2
4,1,2
5,1,2
6,1,2
1,2,2
6,2,2
1,3,2
2,3,2
3,3,2
4,3,2
5,3,2
6,3,2
1,1,3
2,1,3
3,1,3
4,1,3
5,1,3
6,1,3
1,2,3
2,2,3
3,2,3
4,2,3
5,2,3
6,2,3
1,3,3
2,3,3
3,3,3
4,3,3
5,3,3
6,3,3";
    const ANS_PT1_3: u64 = 108;
    const ANS_PT2_1: u64 = 58;
    const ANS_PT2_3: u64 = 90;

    test_solution!(test1_1, solve_pt1, 64, TEST_DATA_1);
    test_solution!(test1_2, solve_pt1, 10, TEST_DATA_2);
    test_solution!(test1_3, solve_pt1, 108, TEST_DATA_3);

    test_solution!(test2_1, solve_pt2, 58, TEST_DATA_1);
    test_solution!(test2_3, solve_pt2, 90, TEST_DATA_3);
}
