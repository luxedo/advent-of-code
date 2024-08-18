/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 09
* Rust Solution
*
* Day 9: Rope Bridge
*
* https://adventofcode.com/2022/day/9
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
enum Direction {
    UP = b'U',
    LEFT = b'L',
    DOWN = b'D',
    RIGHT = b'R',
}

struct Move {
    direction: Direction,
    steps: i32,
}

#[derive(Debug, Copy, Clone)]
struct Pos {
    x: i32,
    y: i32,
}

fn parse_move(move_str: &str) -> Move {
    let (direction, steps) = move_str.split_once(" ").unwrap();
    Move {
        direction: match direction.chars().next().unwrap() {
            'U' => Direction::UP,
            'L' => Direction::LEFT,
            'D' => Direction::DOWN,
            'R' => Direction::RIGHT,
            d => panic!("Direction {d} not found"),
        },
        steps: steps.parse().unwrap(),
    }
}

fn parse_input(input_text: &str) -> Vec<Move> {
    input_text.lines().map(parse_move).collect::<Vec<_>>()
}

fn move_rope(moves: Vec<Move>, knots: u32) -> Vec<Vec<Pos>> {
    let mut rope = vec![vec![Pos { x: 0, y: 0 }]; knots.try_into().unwrap()];
    let mut head = rope[0][0];

    moves.iter().for_each(|mv| {
        (0..mv.steps).for_each(|_| {
            head = move_head(&head, mv.direction);
            rope[0].push(head);
            (1..knots as usize).for_each(|k| {
                let prev_knot = *rope[k - 1].last().unwrap();
                let mut cur_knot = *rope[k].last().unwrap();
                cur_knot = follow_tail(&cur_knot, &prev_knot);
                rope[k].push(cur_knot);
            })
        })
    });
    rope
}

fn move_head(head: &Pos, direction: Direction) -> Pos {
    match direction {
        Direction::UP => Pos {
            x: head.x,
            y: head.y + 1,
        },
        Direction::LEFT => Pos {
            x: head.x - 1,
            y: head.y,
        },
        Direction::DOWN => Pos {
            x: head.x,
            y: head.y - 1,
        },
        Direction::RIGHT => Pos {
            x: head.x + 1,
            y: head.y,
        },
    }
}

fn follow_tail(tail: &Pos, head: &Pos) -> Pos {
    let (dx, dy) = (tail.x - head.x, tail.y - head.y);
    match (dx.abs(), dy.abs()) {
        (x, y) if x <= 1 && y <= 1 => *tail,
        (0, _) => Pos {
            x: tail.x,
            y: tail.y - 1 * dy.signum(),
        },
        (_, 0) => Pos {
            x: tail.x - 1 * dx.signum(),
            y: tail.y,
        },
        (_, _) => Pos {
            x: tail.x - 1 * dx.signum(),
            y: tail.y - 1 * dy.signum(),
        },
    }
}

fn board_size(positions: &Vec<Pos>) -> (i32, i32, i32, i32, i32, i32) {
    let (x0, x1) = (
        positions.iter().map(|p| p.x).min().unwrap(),
        positions.iter().map(|p| p.x).max().unwrap(),
    );
    let (y0, y1) = (
        positions.iter().map(|p| p.y).min().unwrap(),
        positions.iter().map(|p| p.y).max().unwrap(),
    );
    let (h, w) = (y1 - y0 + 1, x1 - x0 + 1);
    (x0, x1, y0, y1, w, h)
}

fn pos_frequency(positions: &Vec<Pos>) -> Vec<Vec<u64>> {
    let (x0, _, y0, _, w, h) = board_size(&positions);
    let mut freq = vec![vec![0; w.try_into().unwrap()]; h.try_into().unwrap()];
    positions.iter().for_each(|p| {
        freq[(p.y - y0) as usize][(p.x - x0) as usize] += 1;
    });
    freq
}

fn solve(input_text: &str, knots: u32) -> u64 {
    let moves = parse_input(&input_text);
    let rope = move_rope(moves, knots);
    let t_freq = pos_frequency(rope.last().unwrap());
    const ONCE: u64 = 1;
    let t_vis_once = t_freq
        .into_iter()
        .fold(0 as u64, |acc, row| {
            acc + row.iter().filter(|c| c >= &&ONCE).count() as u64
        })
        .try_into()
        .unwrap();
    t_vis_once
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const KNOTS: u32 = 2;
    Ok(solve(input_data, KNOTS))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const KNOTS: u32 = 10;
    Ok(solve(input_data, KNOTS))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
