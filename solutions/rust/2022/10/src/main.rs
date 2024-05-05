/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 10
* Rust Solution
*
* Day 10: Cathode-Ray Tube
*
* https://adventofcode.com/2022/day/10
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

#[derive(Debug)]
enum Instr {
    ADDX,
    NOOP,
}

#[derive(Debug)]
struct Instruction {
    instruction: Instr,
    argument: i64,
}
impl Instruction {
    fn from_tuple((instr, arg): (&str, &str)) -> Self {
        match instr {
            "addx" => Instruction {
                instruction: Instr::ADDX,
                argument: arg.parse::<i64>().unwrap(),
            },
            "noop" => Instruction {
                instruction: Instr::NOOP,
                argument: 0,
            },
            _ => panic!("Instruction {instr} not found"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Register {
    value: i64,
    cycles: u64,
}

#[derive(Debug, Copy, Clone)]
struct CPU {
    register_x: Register,
}

impl CPU {
    fn noop(&mut self) -> Vec<Register> {
        let cur_reg = self.register_x;
        self.register_x.cycles += 1;
        vec![cur_reg]
    }
    fn addx(&mut self, argument: i64) -> Vec<Register> {
        let cur_reg = self.register_x.clone();
        let mut next_reg = self.register_x.clone();
        next_reg.cycles += 1;
        self.register_x.cycles += 2;
        self.register_x.value += argument;
        vec![cur_reg, next_reg]
    }
    fn execute(&mut self, i: &Instruction) -> Vec<Register> {
        match i.instruction {
            Instr::ADDX => self.addx(i.argument),
            Instr::NOOP => self.noop(),
        }
    }
}

fn run_program(instructions: &Vec<Instruction>) -> Vec<Register> {
    let mut cpu = CPU {
        register_x: Register {
            value: 1, // Register starts at 1
            cycles: 0,
        },
    };
    let mut ret = cpu.noop(); // Starts at cycle 1
    ret.extend(
        instructions
            .iter()
            .map(|instruction| cpu.execute(instruction))
            .flatten()
            .collect::<Vec<_>>(),
    );
    ret
}

fn parse_instructions(input_text: &str) -> Vec<Instruction> {
    input_text
        .lines()
        .map(|line| Instruction::from_tuple(line.split_once(" ").unwrap_or((line, ""))))
        .collect::<Vec<_>>()
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let instructions = parse_instructions(input_data);
    let history = run_program(&instructions);
    const STARTING_CYCLE: usize = 20;
    const CHUNK_SIZE: usize = 40;
    let signal_strength = history[STARTING_CYCLE..]
        .chunks(CHUNK_SIZE)
        .map(|chunk| chunk[0].value * chunk[0].cycles as i64)
        .sum::<i64>();
    Ok(signal_strength)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let instructions = parse_instructions(input_data);
    let history = run_program(&instructions);
    const SCREEN_WIDTH: usize = 40;
    let screen = history[1..]
        .chunks(SCREEN_WIDTH)
        .map(|chunk| {
            chunk
                .iter()
                .enumerate()
                .map(|(i, r)| {
                    if ((r.value - 1) <= i as i64) && ((r.value + 1) >= i as i64) {
                        '#'
                    } else {
                        '.'
                    }
                })
                .collect::<String>()
                + "\n"
        })
        .collect::<String>();
    Ok(screen)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
