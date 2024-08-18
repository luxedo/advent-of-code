/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 12
* Rust Solution
*
* Day 12: Hill Climbing Algorithm
*
* https://adventofcode.com/2022/day/12
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
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

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const START_MARKER: u8 = b'S';
    const END_MARKER: u8 = b'E';
    let mut board = parse_input(&input_data, START_MARKER, END_MARKER);
    let solution = board.solve().unwrap();
    // board.print_moves(&solution);
    Ok(solution.len() as u64 - 1)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const START_MARKER: u8 = b'S';
    const END_MARKER: u8 = b'E';
    let original = parse_input(&input_data, START_MARKER, END_MARKER);
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

    // original.print_moves(&solution);
    Ok(solution.len() as u64 - 1)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
