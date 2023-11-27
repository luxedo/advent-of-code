/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/22
 *
 *
 * --- Day 21: Monkey Math ---
 * The monkeys are back! You're worried they're going to try to steal your stuff again, but it seems like they're just holding their ground and making various monkey noises at you.
 *
 * Eventually, one of the elephants realizes you don't speak monkey and comes over to interpret. As it turns out, they overheard you talking about trying to find the grove; they can show you a shortcut if you answer their riddle.
 *
 * Each monkey is given a job: either to yell a specific number or to yell the result of a math operation. All of the number-yelling monkeys know their number from the start; however, the math operation monkeys need to wait for two other monkeys to yell a number, and those two other monkeys might also be waiting on other monkeys.
 *
 * Your job is to work out the number the monkey named root will yell before the monkeys figure it out themselves.
 *
 * For example:
 *
 * root: pppw + sjmn
 * dbpl: 5
 * cczh: sllz + lgvd
 * zczc: 2
 * ptdq: humn - dvpt
 * dvpt: 3
 * lfqf: 4
 * humn: 5
 * ljgn: 2
 * sjmn: drzm * dbpl
 * sllz: 4
 * pppw: cczh / lfqf
 * lgvd: ljgn * ptdq
 * drzm: hmdt - zczc
 * hmdt: 32
 * Each line contains the name of a monkey, a colon, and then the job of that monkey:
 *
 * A lone number means the monkey's job is simply to yell that number.
 * A job like aaaa + bbbb means the monkey waits for monkeys aaaa and bbbb to yell each of their numbers; the monkey then yells the sum of those two numbers.
 * aaaa - bbbb means the monkey yells aaaa's number minus bbbb's number.
 * Job aaaa * bbbb will yell aaaa's number multiplied by bbbb's number.
 * Job aaaa / bbbb will yell aaaa's number divided by bbbb's number.
 * So, in the above example, monkey drzm has to wait for monkeys hmdt and zczc to yell their numbers. Fortunately, both hmdt and zczc have jobs that involve simply yelling a single number, so they do this immediately: 32 and 2. Monkey drzm can then yell its number by finding 32 minus 2: 30.
 *
 * Then, monkey sjmn has one of its numbers (30, from monkey drzm), and already has its other number, 5, from dbpl. This allows it to yell its own number by finding 30 multiplied by 5: 150.
 *
 * This process continues until root yells a number: 152.
 *
 * However, your actual situation involves considerably more monkeys. What number will the monkey named root yell?
 *
 * --- Part Two ---
 * Due to some kind of monkey-elephant-human mistranslation, you seem to have misunderstood a few key details about the riddle.
 *
 * First, you got the wrong job for the monkey named root; specifically, you got the wrong math operation. The correct operation for monkey root should be =, which means that it still listens for two numbers (from the same two monkeys as before), but now checks that the two numbers match.
 *
 * Second, you got the wrong monkey for the job starting with humn:. It isn't a monkey - it's you. Actually, you got the job wrong, too: you need to figure out what number you need to yell so that root's equality check passes. (The number that appears after humn: in your input is now irrelevant.)
 *
 * In the above example, the number you need to yell to pass root's equality test is 301. (This causes root to get the same number, 150, from both of its monkeys.)
 *
 * What number do you yell to pass root's equality test?
 */

use aoc_rust::load_input;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt;
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

fn solve_pt1(input_text: &str) -> u64 {
    let math = input_text.parse::<MonkeyMath>().unwrap();
    let root = "root".parse::<Name>().unwrap();
    math.calc(&root).unwrap() as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    let mut math = input_text.parse::<MonkeyMath>().unwrap();
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
    math.rev_calc(&humn, &humn_child, value) as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_21_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 286698846151845

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 3759566892641
    // Correct: 8578625219206 Too High

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";

    // Sub
    // a)
    // ptdq = 10
    // humn = 12
    // humn - ptdq
    // dvpt = 2

    // b)
    // ptdq = 10
    // dvpt = 2
    // dvpt + ptdq
    // humn = 12

    // Div
    // a)
    // pppw = 10
    // cchz = 20
    // cchz/pppw
    // lfqf = 2

    // b)
    // pppw = 10
    // lfqf = 2
    // lfqf * pppw
    // cchz = 20

    test_solution!(test1, solve_pt1, 152, TEST_DATA);
    test_solution!(test2, solve_pt2, 301, TEST_DATA);
}
