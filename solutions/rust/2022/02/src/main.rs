/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 02
* Rust Solution
*
* Day 2: Rock Paper Scissors
*
* https://adventofcode.com/2022/day/2
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

// Dá pra fazer com enum?
const E_ROCK: &str = "A";
const E_PAPER: &str = "B";
const E_SCISSORS: &str = "C";
const P_ROCK: &str = "X";
const P_PAPER: &str = "Y";
const P_SCISSORS: &str = "Z";
const P_LOSE: &str = "X";
const P_DRAW: &str = "Y";
const P_WIN: &str = "Z";

#[derive(Debug, Copy, Clone, PartialEq)]
enum Move {
    ROCK = 1,
    PAPER = 2,
    SCISSORS = 3,
}

#[derive(Debug)]
enum Outcome {
    LOSE = 0,
    DRAW = 3,
    WIN = 6,
}

fn parse_move(str_move: &str) -> Move {
    match str_move {
        E_ROCK | P_ROCK => Move::ROCK,
        E_PAPER | P_PAPER => Move::PAPER,
        E_SCISSORS | P_SCISSORS => Move::SCISSORS,
        _ => panic!("Oh no! Move not found!"),
    }
}

fn parse_condition(str_condition: &str) -> Outcome {
    match str_condition {
        P_LOSE => Outcome::LOSE,
        P_DRAW => Outcome::DRAW,
        P_WIN => Outcome::WIN,
        &_ => panic!("Oh no! Outcome not found!"),
    }
}

fn parse_strategy1(str_game: &str) -> (Move, Move) {
    let moves = str_game.split_whitespace().collect::<Vec<&str>>();
    (parse_move(moves[0]), parse_move(moves[1]))
}

fn parse_strategy2(str_game: &str) -> (Move, Outcome) {
    let moves = str_game.split_whitespace().collect::<Vec<&str>>();
    (parse_move(moves[0]), parse_condition(moves[1]))
}

fn play(enemy_move: Move, player_move: Move) -> Outcome {
    match enemy_move {
        _ if enemy_move == player_move => Outcome::DRAW,
        Move::ROCK => match player_move {
            Move::PAPER => Outcome::WIN,
            _ => Outcome::LOSE,
        },
        Move::PAPER => match player_move {
            Move::SCISSORS => Outcome::WIN,
            _ => Outcome::LOSE,
        },
        Move::SCISSORS => match player_move {
            Move::ROCK => Outcome::WIN,
            _ => Outcome::LOSE,
        },
    }
}

fn score((enemy_move, player_move): (Move, Move)) -> u32 {
    let outcome = play(enemy_move, player_move);
    outcome as u32 + player_move as u32
}

fn find_move(enemy_move: Move, condition: Outcome) -> Move {
    match condition {
        Outcome::DRAW => enemy_move,
        Outcome::LOSE => match enemy_move {
            Move::ROCK => Move::SCISSORS,
            Move::PAPER => Move::ROCK,
            Move::SCISSORS => Move::PAPER,
        },
        Outcome::WIN => match enemy_move {
            Move::ROCK => Move::PAPER,
            Move::PAPER => Move::SCISSORS,
            Move::SCISSORS => Move::ROCK,
        },
    }
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let answer = input_data
        .trim_end_matches("\n")
        .split("\n")
        .map(parse_strategy1)
        .map(score)
        .sum::<u32>();
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let answer = input_data
        .trim_end_matches("\n")
        .split("\n")
        .map(parse_strategy2)
        .map(|game| (game.0, find_move(game.0, game.1)))
        .map(score)
        .sum::<u32>();
    Ok(answer)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
