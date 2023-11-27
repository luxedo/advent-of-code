/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/12
 *
 * --- Day 12: Hill Climbing Algorithm ---
 * You try contacting the Elves using your handheld device, but the river you're following must be too low to get a decent signal.
 *
 * You ask the device for a heightmap of the surrounding area (your puzzle input). The heightmap shows the local area from above broken into a grid; the elevation of each square of the grid is given by a single lowercase letter, where a is the lowest elevation, b is the next-lowest, and so on up to the highest elevation, z.
 *
 * Also included on the heightmap are marks for your current position (S) and the location that should get the best signal (E). Your current position (S) has elevation a, and the location that should get the best signal (E) has elevation z.
 *
 * You'd like to reach E, but to save energy, you should do it in as few steps as possible. During each step, you can move exactly one square up, down, left, or right. To avoid needing to get out your climbing gear, the elevation of the destination square can be at most one higher than the elevation of your current square; that is, if your current elevation is m, you could step to elevation n, but not to elevation o. (This also means that the elevation of the destination square can be much lower than the elevation of your current square.)
 *
 * For example:
 *
 * Sabqponm
 * abcryxxl
 * accszExk
 * acctuvwj
 * abdefghi
 * Here, you start in the top-left corner; your goal is near the middle. You could start by moving down or right, but eventually you'll need to head toward the e at the bottom. From there, you can spiral around to the goal:
 *
 * v..v<<<<
 * >v.vv<<^
 * .>vv>E^^
 * ..v>>>^^
 * ..>>>>>^
 * In the above diagram, the symbols indicate whether the path exits each square moving up (^), down (v), left (<), or right (>). The location that should get the best signal is still E, and . marks unvisited squares.
 *
 * This path reaches the goal in 31 steps, the fewest possible.
 *
 * What is the fewest steps required to move from your current position to the location that should get the best signal?
 *
 *
 * --- Part Two ---
 * As you walk up the hill, you suspect that the Elves will want to turn this into a hiking trail. The beginning isn't very scenic, though; perhaps you can find a better starting point.
 *
 * To maximize exercise while hiking, the trail should start as low as possible: elevation a. The goal is still the square marked E. However, the trail should still be direct, taking the fewest steps to reach its goal. So, you'll need to find the shortest path from any square at elevation a to the square marked E.
 *
 * Again consider the example from above:
 *
 * Sabqponm
 * abcryxxl
 * accszExk
 * acctuvwj
 * abdefghi
 * Now, there are six choices for starting position (five marked a, plus the square marked S that counts as being at elevation a). If you start at the bottom-left square, you can reach the goal most quickly:
 *
 * ...v<<<<
 * ...vv<<^
 * ...v>E^^
 * .>v>>>^^
 * >^>>>>>^
 * This path reaches the goal in only 29 steps, the fewest possible.
 *
 * What is the fewest steps required to move starting from any square with elevation a to the location that should get the best signal?
 */

use aoc_rust::load_input;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::BinaryHeap;
use std::error::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Coord {
    y: usize,
    x: usize,
}

#[derive(Debug, Copy, Clone)]
struct Node {
    id: Option<usize>,
    position: Coord,
    steps: usize,
    parent_id: Option<usize>,
}

impl Eq for Node {}
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .steps // We invert the comparison to pop the lowest first
            .cmp(&self.steps)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone)]
struct Board {
    board: Vec<Vec<u8>>,
    height: usize,
    width: usize,
    start_marker: u8,
    end_marker: u8,
    lowest: u8,
    highest: u8,
    max_step: u8,
}

impl Board {
    fn new(b_array: Vec<Vec<u8>>, start_marker: u8, end_marker: u8) -> Self {
        let (height, width) = (b_array.len(), b_array[0].len());
        let lowest = b'a';
        let highest = b'z';
        let max_step: u8 = 1;

        Self {
            board: b_array,
            height,
            width,
            start_marker,
            end_marker,
            lowest,
            highest,
            max_step,
        }
    }
    fn find_u8(&self, c: u8) -> Vec<Coord> {
        self.board
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .filter(|(_, _c)| c == **_c)
                    .map(move |(x, _)| Coord { y, x })
            })
            .collect()
    }

    fn get_height(&self, y: usize, x: usize) -> u8 {
        match self.board[y][x] {
            m if m == self.start_marker => self.lowest,
            m if m == self.end_marker => self.highest,
            m => m,
        }
    }

    fn possible_coords(&self, position: &Coord) -> Vec<Coord> {
        let Coord { x, y } = *position;
        let max_height = self.get_height(y, x) + self.max_step;
        vec![
            y.checked_sub(1).and_then(|y| Some(Coord { x, y })),
            x.checked_sub(1).and_then(|x| Some(Coord { x, y })),
            match y.cmp(&(self.height - 1)) {
                Ordering::Less => Some(Coord { x, y: y + 1 }),
                _ => None,
            },
            match x.cmp(&(self.width - 1)) {
                Ordering::Less => Some(Coord { x: x + 1, y }),
                _ => None,
            },
        ]
        .into_iter()
        .map(|c| match c {
            Some(Coord { x: new_x, y: new_y }) => {
                match self.get_height(new_y, new_x).cmp(&max_height) {
                    Ordering::Less | Ordering::Equal => c,
                    _ => None,
                }
            }
            _ => None,
        })
        .filter(|c| c.is_some())
        .map(|c| c.unwrap())
        .collect()
    }

    fn build_path(nodes: Vec<Node>, end: Coord) -> Option<Vec<Coord>> {
        let mut last = *nodes.iter().find(|s| s.position == end).unwrap();
        let mut result: Vec<Coord> = vec![last.position];

        while let Some(parent) = nodes.iter().find(|s| s.id == last.parent_id) {
            last = *parent;
            result.push(last.position);
        }
        Some(result)
    }

    fn solve(&mut self) -> Option<Vec<Coord>> {
        let start = self.find_u8(self.start_marker)[0];
        let end = self.find_u8(self.end_marker)[0];
        let start = Coord {
            x: start.x,
            y: start.y,
        };
        let end = Coord { x: end.x, y: end.y };
        let start_state = Node {
            id: None,
            steps: 0,
            position: start,
            parent_id: None,
        };

        let mut checked: BTreeSet<Coord> = BTreeSet::new();
        let mut nodes: BTreeSet<Node> = BTreeSet::new();
        let mut frontier_heap: BinaryHeap<Node> = BinaryHeap::new();
        let mut frontier_nodes: BTreeSet<Node> = BTreeSet::new();
        frontier_heap.push(start_state);
        frontier_nodes.insert(start_state);

        while let Some(mut node) = frontier_heap.pop() {
            // Do the dishes
            let checked_len = &checked.len();
            node.id = Some(checked_len + 1);
            nodes.insert(node);
            checked.insert(node.position);
            frontier_nodes.remove(&node);
            let Node {
                id,
                steps,
                position,
                parent_id: _,
            } = node;

            // Found node! GUARDS!
            if position == end {
                let checked_nodes = nodes.iter().copied().collect::<Vec<Node>>();
                return Self::build_path(checked_nodes, end);
            }

            // Search new paths
            let possible = self
                .possible_coords(&position)
                .into_iter()
                .map(|c| Node {
                    id: None,
                    steps: steps + 1,
                    position: c,
                    parent_id: id,
                })
                .filter(|n| !checked.contains(&n.position));

            // Add new paths to queue
            for node in possible {
                match frontier_nodes.clone().into_iter().find(|n| *n == node) {
                    Some(_) => {
                        // Node already in queue
                        ();
                    }
                    None => {
                        // Add new node to queue
                        frontier_heap.push(node);
                        frontier_nodes.insert(node);
                    }
                };
            }
        }

        None
    }

    fn print_moves(&self, moves: &Vec<Coord>) {
        let mut board = vec![vec!['.'; self.width]; self.height];

        fn find_direction(cur: &Coord, prev: &Coord) -> char {
            if cur == prev {
                panic!("Can't find direction");
            }
            if cur.x == prev.x {
                if cur.y < prev.y {
                    '^'
                } else {
                    'v'
                }
            } else if cur.y == prev.y {
                if cur.x < prev.x {
                    '<'
                } else {
                    '>'
                }
            } else {
                panic!("Can't find direction");
            }
        }
        moves
            .iter()
            .zip(moves[1..].iter())
            .for_each(|(cur, prev)| board[prev.y][prev.x] = find_direction(cur, prev));
        board
            .iter()
            .for_each(|row| println!("{}", row.iter().collect::<String>()));
    }
}

fn parse_input(input_text: &str, start_marker: u8, end_marker: u8) -> Board {
    Board::new(
        input_text
            .lines()
            .map(|line| line.as_bytes().iter().map(|c| *c).collect::<Vec<_>>())
            .collect::<Vec<_>>(),
        start_marker,
        end_marker,
    )
}

fn solve_pt1(input_text: &str) -> u64 {
    const START_MARKER: u8 = b'S';
    const END_MARKER: u8 = b'E';
    let mut board = parse_input(&input_text, START_MARKER, END_MARKER);
    let solution = board.solve().unwrap();
    board.print_moves(&solution);
    solution.len() as u64 - 1
}

fn solve_pt2(input_text: &str) -> u64 {
    const START_MARKER: u8 = b'S';
    const END_MARKER: u8 = b'E';
    let original = parse_input(&input_text, START_MARKER, END_MARKER);
    let start = original.find_u8(START_MARKER)[0];
    const CANDIDATE_HEIGHT: u8 = b'a';
    let start_candidates = original.find_u8(CANDIDATE_HEIGHT);
    let solutions = start_candidates
        .iter()
        .map(|candidate| {
            let mut board = original.clone();
            board.board[start.y][start.x] = CANDIDATE_HEIGHT;
            board.board[candidate.y][candidate.x] = START_MARKER;
            board.solve()
        })
        .filter_map(|s| s)
        .collect::<Vec<Vec<Coord>>>();

    let solution = solutions
        .iter()
        .min_by(|a, b| a.len().cmp(&b.len()))
        .unwrap();

    original.print_moves(&solution);
    solution.len() as u64 - 1
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_12_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 350

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 349

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

    test_solution!(test1, solve_pt1, 31, TEST_DATA);
    test_solution!(test2, solve_pt2, 29, TEST_DATA);
}
