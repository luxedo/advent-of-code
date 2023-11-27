/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/14
 *
 * --- Day 14: Regolith Reservoir ---
 * The distress signal leads you to a giant waterfall! Actually, hang on - the signal seems like it's coming from the waterfall itself, and that doesn't make any sense. However, you do notice a little path that leads behind the waterfall.
 *
 * Correction: the distress signal leads you behind a giant waterfall! There seems to be a large cave system here, and the signal definitely leads further inside.
 *
 * As you begin to make your way deeper underground, you feel the ground rumble for a moment. Sand begins pouring into the cave! If you don't quickly figure out where the sand is going, you could quickly become trapped!
 *
 * Fortunately, your familiarity with analyzing the path of falling material will come in handy here. You scan a two-dimensional vertical slice of the cave above you (your puzzle input) and discover that it is mostly air with structures made of rock.
 *
 * Your scan traces the path of each solid rock structure and reports the x,y coordinates that form the shape of the path, where x represents distance to the right and y represents distance down. Each path appears as a single line of text in your scan. After the first point of each path, each point indicates the end of a straight horizontal or vertical line to be drawn from the previous point. For example:
 *
 * 498,4 -> 498,6 -> 496,6
 * 503,4 -> 502,4 -> 502,9 -> 494,9
 * This scan means that there are two paths of rock; the first path consists of two straight lines, and the second path consists of three straight lines. (Specifically, the first path consists of a line of rock from 498,4 through 498,6 and another line of rock from 498,6 through 496,6.)
 *
 * The sand is pouring into the cave from point 500,0.
 *
 * Drawing rock as #, air as ., and the source of the sand as +, this becomes:
 *
 *
 *   4     5  5
 *   9     0  0
 *   4     0  3
 * 0 ......+...
 * 1 ..........
 * 2 ..........
 * 3 ..........
 * 4 ....#...##
 * 5 ....#...#.
 * 6 ..###...#.
 * 7 ........#.
 * 8 ........#.
 * 9 #########.
 * Sand is produced one unit at a time, and the next unit of sand is not produced until the previous unit of sand comes to rest. A unit of sand is large enough to fill one tile of air in your scan.
 *
 * A unit of sand always falls down one step if possible. If the tile immediately below is blocked (by rock or sand), the unit of sand attempts to instead move diagonally one step down and to the left. If that tile is blocked, the unit of sand attempts to instead move diagonally one step down and to the right. Sand keeps moving as long as it is able to do so, at each step trying to move down, then down-left, then down-right. If all three possible destinations are blocked, the unit of sand comes to rest and no longer moves, at which point the next unit of sand is created back at the source.
 *
 * So, drawing sand that has come to rest as o, the first unit of sand simply falls straight down and then stops:
 *
 * ......+...
 * ..........
 * ..........
 * ..........
 * ....#...##
 * ....#...#.
 * ..###...#.
 * ........#.
 * ......o.#.
 * #########.
 * The second unit of sand then falls straight down, lands on the first one, and then comes to rest to its left:
 *
 * ......+...
 * ..........
 * ..........
 * ..........
 * ....#...##
 * ....#...#.
 * ..###...#.
 * ........#.
 * .....oo.#.
 * #########.
 * After a total of five units of sand have come to rest, they form this pattern:
 *
 * ......+...
 * ..........
 * ..........
 * ..........
 * ....#...##
 * ....#...#.
 * ..###...#.
 * ......o.#.
 * ....oooo#.
 * #########.
 * After a total of 22 units of sand:
 *
 * ......+...
 * ..........
 * ......o...
 * .....ooo..
 * ....#ooo##
 * ....#ooo#.
 * ..###ooo#.
 * ....oooo#.
 * ...ooooo#.
 * #########.
 * Finally, only two more units of sand can possibly come to rest:
 *
 * ......+...
 * ..........
 * ......o...
 * .....ooo..
 * ....#ooo##
 * ...o#ooo#.
 * ..###ooo#.
 * ....oooo#.
 * .o.ooooo#.
 * #########.
 * Once all 24 units of sand shown above have come to rest, all further sand flows out the bottom, falling into the endless void. Just for fun, the path any new sand takes before falling forever is shown here with ~:
 *
 * .......+...
 * .......~...
 * ......~o...
 * .....~ooo..
 * ....~#ooo##
 * ...~o#ooo#.
 * ..~###ooo#.
 * ..~..oooo#.
 * .~o.ooooo#.
 * ~#########.
 * ~..........
 * ~..........
 * ~..........
 * Using your scan, simulate the falling sand. How many units of sand come to rest before sand starts flowing into the abyss below?
 *
 *
 * --- Part Two ---
 * You realize you misread the scan. There isn't an endless void at the bottom of the scan - there's floor, and you're standing on it!
 *
 * You don't have time to scan the floor, so assume the floor is an infinite horizontal line with a y coordinate equal to two plus the highest y coordinate of any point in your scan.
 *
 * In the example above, the highest y coordinate of any point is 9, and so the floor is at y=11. (This is as if your scan contained one extra rock path like -infinity,11 -> infinity,11.) With the added floor, the example above now looks like this:
 *
 *         ...........+........
 *         ....................
 *         ....................
 *         ....................
 *         .........#...##.....
 *         .........#...#......
 *         .......###...#......
 *         .............#......
 *         .............#......
 *         .....#########......
 *         ....................
 * <-- etc #################### etc -->
 * To find somewhere safe to stand, you'll need to simulate falling sand until a unit of sand comes to rest at 500,0, blocking the source entirely and stopping the flow of sand into the cave. In the example above, the situation finally looks like this after 93 units of sand come to rest:
 *
 * ............o............
 * ...........ooo...........
 * ..........ooooo..........
 * .........ooooooo.........
 * ........oo#ooo##o........
 * .......ooo#ooo#ooo.......
 * ......oo###ooo#oooo......
 * .....oooo.oooo#ooooo.....
 * ....oooooooooo#oooooo....
 * ...ooo#########ooooooo...
 * ..ooooo.......ooooooooo..
 * #########################
 * Using your scan, simulate the falling sand until the source of the sand becomes blocked. How many units of sand come to rest?
 */

use aoc_rust::load_input;
use std::error::Error;
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

fn solve_pt1(input_text: &str) -> u64 {
    let mut board = Board::from_str(input_text);
    const SAND_X: usize = 500;
    const SAND_Y: usize = 0;
    board.drop_sand_until_fall(SAND_X, SAND_Y) as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    let mut board = Board::from_str(input_text);
    const SAND_X: usize = 500;
    const SAND_Y: usize = 0;
    board.drop_sand_until_full(SAND_X, SAND_Y) as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_14_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 961

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 26375

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

    test_solution!(test1, solve_pt1, 24, TEST_DATA);
    test_solution!(test2, solve_pt2, 93, TEST_DATA);
}
