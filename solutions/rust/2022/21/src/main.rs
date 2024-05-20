/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 21
* Rust Solution
*
* Day 21: Monkey Math
*
* https://adventofcode.com/2022/day/21
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
struct Name(char, char, char, char);
impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}{}", self.0, self.1, self.2, self.3)
    }
}
#[derive(Debug)]
struct ParseNameError;
impl FromStr for Name {
    type Err = ParseNameError;
    fn from_str(name: &str) -> Result<Name, Self::Err> {
        let mut name = name.chars();
        Ok(Name(
            name.next().ok_or(ParseNameError).unwrap(),
            name.next().ok_or(ParseNameError).unwrap(),
            name.next().ok_or(ParseNameError).unwrap(),
            name.next().ok_or(ParseNameError).unwrap(),
        ))
    }
}

#[derive(Clone, Copy)]
enum Job {
    Yell(isize),
    Add(Name, Name),
    Sub(Name, Name),
    Mul(Name, Name),
    Div(Name, Name),
}
impl fmt::Debug for Job {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Yell(value) => write!(f, "{}", value),
            Self::Add(v1, v2) => write!(f, "{:?} + {:?}", v1, v2),
            Self::Sub(v1, v2) => write!(f, "{:?} - {:?}", v1, v2),
            Self::Mul(v1, v2) => write!(f, "{:?} * {:?}", v1, v2),
            Self::Div(v1, v2) => write!(f, "{:?} / {:?}", v1, v2),
        }
    }
}
#[derive(Debug)]
struct ParseJobError;
impl FromStr for Job {
    type Err = ParseJobError;
    fn from_str(job: &str) -> Result<Job, Self::Err> {
        fn names_in_job(job: &str, op: &str) -> (Option<Name>, Option<Name>) {
            let (name1, name2) = job.split_once(op).unwrap();
            (name1.parse::<Name>().ok(), name2.parse::<Name>().ok())
        }
        Ok(
            match (
                job.contains('+'),
                job.contains('-'),
                job.contains('*'),
                job.contains('/'),
            ) {
                (true, ..) => {
                    let (s1, s2) = names_in_job(&job, " + ");
                    Job::Add(s1.unwrap(), s2.unwrap())
                }
                (_, true, ..) => {
                    let (s1, s2) = names_in_job(&job, " - ");
                    Job::Sub(s1.unwrap(), s2.unwrap())
                }
                (.., true, _) => {
                    let (s1, s2) = names_in_job(&job, " * ");
                    Job::Mul(s1.unwrap(), s2.unwrap())
                }
                (.., true) => {
                    let (s1, s2) = names_in_job(&job, " / ");
                    Job::Div(s1.unwrap(), s2.unwrap())
                }
                _ => Job::Yell(job.parse::<isize>().unwrap()),
            },
        )
    }
}

#[derive(Debug)]
struct MonkeyMath {
    monkeys: BTreeMap<Name, Job>,
}
#[derive(Debug)]
struct ParseMonkeyMathError;
impl FromStr for MonkeyMath {
    type Err = ParseMonkeyMathError;
    fn from_str(input_text: &str) -> Result<MonkeyMath, Self::Err> {
        let monkeys: BTreeMap<Name, Job> = BTreeMap::from_iter(input_text.lines().map(|line| {
            let (name, job) = line.split_once(": ").unwrap();
            let name = name.parse::<Name>().unwrap();
            let job = job.parse::<Job>().unwrap();
            (name, job)
        }));
        Ok(MonkeyMath { monkeys })
    }
}

impl MonkeyMath {
    fn get(&self, name: &Name) -> Option<Job> {
        self.monkeys.get(name).copied()
    }
    fn calc(&self, name: &Name) -> Option<isize> {
        self.monkeys.get(name).and_then(|job| match job {
            Job::Yell(value) => Some(*value),
            Job::Add(name1, name2) => self
                .calc(name1)
                .and_then(|value1| self.calc(name2).and_then(|value2| Some(value1 + value2))),
            Job::Sub(name1, name2) => self
                .calc(name1)
                .and_then(|value1| self.calc(name2).and_then(|value2| Some(value1 - value2))),
            Job::Mul(name1, name2) => self
                .calc(name1)
                .and_then(|value1| self.calc(name2).and_then(|value2| Some(value1 * value2))),
            Job::Div(name1, name2) => self
                .calc(name1)
                .and_then(|value1| self.calc(name2).and_then(|value2| Some(value1 / value2))),
        })
    }
    fn in_chain(&self, name: &Name, chain: &Name) -> bool {
        if chain == name {
            return true;
        }
        match self.get(chain) {
            Some(Job::Yell(_)) => false,
            Some(Job::Add(name1, name2))
            | Some(Job::Sub(name1, name2))
            | Some(Job::Mul(name1, name2))
            | Some(Job::Div(name1, name2)) => {
                self.in_chain(name, &name1) || self.in_chain(name, &name2)
            }
            None => false,
        }
    }
    fn rev_calc(&self, unknown: &Name, chain: &Name, value: isize) -> isize {
        if unknown == chain {
            return value;
        }
        let (child1, child2) = self.get_children(chain);
        let (child1, child2) = (child1.unwrap(), child2.unwrap());
        let (unk_child, other_child) = if self.in_chain(&unknown, &child1) {
            (child1, child2)
        } else {
            (child2, child1)
        };
        let other_value = self.calc(&other_child).unwrap();
        let value = match (self.get(chain), child1 == unk_child) {
            (Some(Job::Add(_, _)), _) => value - other_value,
            (Some(Job::Sub(_, _)), true) => value + other_value,
            (Some(Job::Sub(_, _)), false) => other_value - value,
            (Some(Job::Mul(_, _)), _) => value / other_value,
            (Some(Job::Div(_, _)), true) => value * other_value,
            (Some(Job::Div(_, _)), false) => other_value / value,
            _ => panic!("You shouldn't be here!"),
        };
        self.rev_calc(unknown, &unk_child, value)
    }
    fn get_children(&self, monkey: &Name) -> (Option<Name>, Option<Name>) {
        match self.get(&monkey) {
            Some(Job::Add(child1, child2))
            | Some(Job::Sub(child1, child2))
            | Some(Job::Mul(child1, child2))
            | Some(Job::Div(child1, child2)) => (Some(child1), Some(child2)),
            _ => (None, None),
        }
    }
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let math = input_data.parse::<MonkeyMath>().unwrap();
    let root = "root".parse::<Name>().unwrap();
    Ok(math.calc(&root).unwrap())
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let math = input_data.parse::<MonkeyMath>().unwrap();
    let root = "root".parse::<Name>().unwrap();
    let humn = "humn".parse::<Name>().unwrap();
    let (child1, child2) = math.get_children(&root);
    let (child1, child2) = (child1.unwrap(), child2.unwrap());
    let (humn_child, other_child) = if math.in_chain(&humn, &child1) {
        (child1, child2)
    } else {
        (child2, child1)
    };
    let value = math.calc(&other_child).unwrap();
    Ok(math.rev_calc(&humn, &humn_child, value))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
