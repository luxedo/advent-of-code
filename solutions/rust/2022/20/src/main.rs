/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 20
* Rust Solution
*
* Day 20: Grove Positioning System
*
* https://adventofcode.com/2022/day/20
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

#[derive(Debug)]
struct Decryptor {
    message: Vec<(usize, isize)>,
    current: Vec<(usize, isize)>,
    rounds: usize,
    length: isize,
    key: isize,
}
impl Decryptor {
    fn run(&mut self, rounds: usize) {
        for _ in 0..rounds {
            let cursor = self.rounds % self.current.len();
            let position = self.current.iter().position(|n| n.0 == cursor).unwrap();
            self.move_number(position);
            self.rounds += 1;
        }
    }
    fn move_number(&mut self, position: usize) {
        let number = self.current[position];
        let total_offset = position as isize + number.1;
        let final_position = total_offset.rem_euclid(self.length - 1);
        self.current.remove(position);
        self.current.insert(final_position as usize, number);
    }
    fn decrypt(&mut self, cycles: usize) -> isize {
        self.reset();
        for _ in 0..cycles {
            self.run(self.length as usize);
        }
        let d0 = self.get_grove(0, 1000);
        let d1 = self.get_grove(0, 2000);
        let d2 = self.get_grove(0, 3000);
        (d0 + d1 + d2) as isize
    }
    fn get_grove(&self, g: isize, o: isize) -> isize {
        let position = self.current.iter().position(|n| n.1 == g).unwrap() as isize;
        let position = (position + o).rem_euclid(self.length) as usize;
        self.current[position].1
    }
    fn reset(&mut self) {
        self.current = self
            .message
            .iter()
            .map(|(i, value)| (*i, value * self.key))
            .collect::<Vec<(usize, isize)>>();
    }
    fn set_key(&mut self, key: isize) {
        self.key = key;
        self.reset();
    }
}

fn parse_input(input_text: &str) -> Decryptor {
    let message = input_text
        .lines()
        .enumerate()
        .map(|(i, line)| (i, line.parse::<isize>().unwrap()))
        .collect::<Vec<(usize, isize)>>();
    let length = message.len() as isize;
    Decryptor {
        current: message.clone(),
        message,
        rounds: 0,
        length,
        key: 1,
    }
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let mut decryptor = parse_input(input_data);
    Ok(decryptor.decrypt(1))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let mut decryptor = parse_input(input_data);
    let key = 811589153;
    let cycles = 10;
    decryptor.set_key(key);
    Ok(decryptor.decrypt(cycles))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
