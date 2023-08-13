/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/16
 *
 * --- Day 16: Proboscidea Volcanium ---
 * The sensors have led you to the origin of the distress signal: yet another handheld device, just like the one the Elves gave you. However, you don't see any Elves around; instead, the device is surrounded by elephants! They must have gotten lost in these tunnels, and one of the elephants apparently figured out how to turn on the distress signal.
 *
 * The ground rumbles again, much stronger this time. What kind of cave is this, exactly? You scan the cave with your handheld device; it reports mostly igneous rock, some ash, pockets of pressurized gas, magma... this isn't just a cave, it's a volcano!
 *
 * You need to get the elephants out of here, quickly. Your device estimates that you have 30 minutes before the volcano erupts, so you don't have time to go back out the way you came in.
 *
 * You scan the cave for other options and discover a network of pipes and pressure-release valves. You aren't sure how such a system got into a volcano, but you don't have time to complain; your device produces a report (your puzzle input) of each valve's flow rate if it were opened (in pressure per minute) and the tunnels you could use to move between the valves.
 *
 * There's even a valve in the room you and the elephants are currently standing in labeled AA. You estimate it will take you one minute to open a single valve and one minute to follow any tunnel from one valve to another. What is the most pressure you could release?
 *
 * For example, suppose you had the following scan output:
 *
 * Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
 * Valve BB has flow rate=13; tunnels lead to valves CC, AA
 * Valve CC has flow rate=2; tunnels lead to valves DD, BB
 * Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
 * Valve EE has flow rate=3; tunnels lead to valves FF, DD
 * Valve FF has flow rate=0; tunnels lead to valves EE, GG
 * Valve GG has flow rate=0; tunnels lead to valves FF, HH
 * Valve HH has flow rate=22; tunnel leads to valve GG
 * Valve II has flow rate=0; tunnels lead to valves AA, JJ
 * Valve JJ has flow rate=21; tunnel leads to valve II
 * All of the valves begin closed. You start at valve AA, but it must be damaged or jammed or something: its flow rate is 0, so there's no point in opening it. However, you could spend one minute moving to valve BB and another minute opening it; doing so would release pressure during the remaining 28 minutes at a flow rate of 13, a total eventual pressure release of 28 * 13 = 364. Then, you could spend your third minute moving to valve CC and your fourth minute opening it, providing an additional 26 minutes of eventual pressure release at a flow rate of 2, or 52 total pressure released by valve CC.
 *
 * Making your way through the tunnels like this, you could probably open many or all of the valves by the time 30 minutes have elapsed. However, you need to release as much pressure as possible, so you'll need to be methodical. Instead, consider this approach:
 *
 * == Minute 1 ==
 * No valves are open.
 * You move to valve DD.
 *
 * == Minute 2 ==
 * No valves are open.
 * You open valve DD.
 *
 * == Minute 3 ==
 * Valve DD is open, releasing 20 pressure.
 * You move to valve CC.
 *
 * == Minute 4 ==
 * Valve DD is open, releasing 20 pressure.
 * You move to valve BB.
 *
 * == Minute 5 ==
 * Valve DD is open, releasing 20 pressure.
 * You open valve BB.
 *
 * == Minute 6 ==
 * Valves BB and DD are open, releasing 33 pressure.
 * You move to valve AA.
 *
 * == Minute 7 ==
 * Valves BB and DD are open, releasing 33 pressure.
 * You move to valve II.
 *
 * == Minute 8 ==
 * Valves BB and DD are open, releasing 33 pressure.
 * You move to valve JJ.
 *
 * == Minute 9 ==
 * Valves BB and DD are open, releasing 33 pressure.
 * You open valve JJ.
 *
 * == Minute 10 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve II.
 *
 * == Minute 11 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve AA.
 *
 * == Minute 12 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve DD.
 *
 * == Minute 13 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve EE.
 *
 * == Minute 14 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve FF.
 *
 * == Minute 15 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve GG.
 *
 * == Minute 16 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You move to valve HH.
 *
 * == Minute 17 ==
 * Valves BB, DD, and JJ are open, releasing 54 pressure.
 * You open valve HH.
 *
 * == Minute 18 ==
 * Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
 * You move to valve GG.
 *
 * == Minute 19 ==
 * Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
 * You move to valve FF.
 *
 * == Minute 20 ==
 * Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
 * You move to valve EE.
 *
 * == Minute 21 ==
 * Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
 * You open valve EE.
 *
 * == Minute 22 ==
 * Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
 * You move to valve DD.
 *
 * == Minute 23 ==
 * Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
 * You move to valve CC.
 *
 * == Minute 24 ==
 * Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
 * You open valve CC.
 *
 * == Minute 25 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * == Minute 26 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * == Minute 27 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * == Minute 28 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * == Minute 29 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * == Minute 30 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 * This approach lets you release the most pressure possible in 30 minutes with this valve layout, 1651.
 *
 * Work out the steps to release the most pressure in 30 minutes. What is the most pressure you can release?
 *
 *
 * --- Part Two ---
 * You're worried that even with an optimal approach, the pressure released won't be enough. What if you got one of the elephants to help you?
 *
 * It would take you 4 minutes to teach an elephant how to open the right valves in the right order, leaving you with only 26 minutes to actually execute your plan. Would having two of you working together be better, even if it means having less time? (Assume that you teach the elephant before opening any valves yourself, giving you both the same full 26 minutes.)
 *
 * In the example above, you could teach the elephant to help you as follows:
 *
 * == Minute 1 ==
 * No valves are open.
 * You move to valve II.
 * The elephant moves to valve DD.
 *
 * == Minute 2 ==
 * No valves are open.
 * You move to valve JJ.
 * The elephant opens valve DD.
 *
 * == Minute 3 ==
 * Valve DD is open, releasing 20 pressure.
 * You open valve JJ.
 * The elephant moves to valve EE.
 *
 * == Minute 4 ==
 * Valves DD and JJ are open, releasing 41 pressure.
 * You move to valve II.
 * The elephant moves to valve FF.
 *
 * == Minute 5 ==
 * Valves DD and JJ are open, releasing 41 pressure.
 * You move to valve AA.
 * The elephant moves to valve GG.
 *
 * == Minute 6 ==
 * Valves DD and JJ are open, releasing 41 pressure.
 * You move to valve BB.
 * The elephant moves to valve HH.
 *
 * == Minute 7 ==
 * Valves DD and JJ are open, releasing 41 pressure.
 * You open valve BB.
 * The elephant opens valve HH.
 *
 * == Minute 8 ==
 * Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
 * You move to valve CC.
 * The elephant moves to valve GG.
 *
 * == Minute 9 ==
 * Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
 * You open valve CC.
 * The elephant moves to valve FF.
 *
 * == Minute 10 ==
 * Valves BB, CC, DD, HH, and JJ are open, releasing 78 pressure.
 * The elephant moves to valve EE.
 *
 * == Minute 11 ==
 * Valves BB, CC, DD, HH, and JJ are open, releasing 78 pressure.
 * The elephant opens valve EE.
 *
 * (At this point, all valves are open.)
 *
 * == Minute 12 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * ...
 *
 * == Minute 20 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 *
 * ...
 *
 * == Minute 26 ==
 * Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
 * With the elephant helping, after 26 minutes, the best you could do would release a total of 1707 pressure.
 *
 * With you and an elephant working together for 26 minutes, what is the most pressure you could release?
 */
use aoc_rust::load_input;
use std::error::Error;
use std::fmt;

#[derive(Clone, Copy, Eq, PartialEq)]
struct Id(char, char);
impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id({}{})", self.0, self.1)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Valve {
    id: usize,
    char_id: Id,
    flow_rate: usize,
    char_tunnels: Vec<Id>,
    tunnels: Vec<usize>,
    distances: Vec<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Node {
    flow_rate: usize,
    time: usize,
    valve_id: usize,
    open_valves: Vec<bool>,
}

fn parse_input(input_text: &str) -> Vec<Valve> {
    let mut valves = input_text
        .lines()
        .enumerate()
        .map(|(id, line)| {
            let split = line.split(" ").collect::<Vec<&str>>();
            let mut char_id = split[1].chars();
            let char_id = Id(char_id.next().unwrap(), char_id.next().unwrap());
            let flow_rate = split[4]
                .trim_start_matches("rate=")
                .trim_end_matches(";")
                .parse::<usize>()
                .unwrap();
            let char_tunnels = split[9..]
                .iter()
                .map(|t| {
                    let mut c = t.chars();
                    Id(c.next().unwrap(), c.next().unwrap())
                })
                .collect::<Vec<Id>>();
            let tunnels = vec![];
            let distances = vec![1; char_tunnels.len()];
            Valve {
                id,
                char_id,
                flow_rate,
                char_tunnels,
                tunnels,
                distances,
            }
        })
        .collect::<Vec<Valve>>();
    let id_map = valves
        .iter()
        .map(|v| (v.char_id, v.id))
        .collect::<Vec<(Id, usize)>>();
    valves.iter_mut().for_each(|v| {
        for char_id in v.char_tunnels.clone() {
            let id = id_map.iter().find(|m| m.0 == char_id).unwrap().1;
            v.tunnels.push(id);
        }
    });
    valves
}

fn shortest_distance(origin: &Valve, dst_idx: usize, valves: &Vec<Valve>) -> usize {
    _shortest_distance(origin, dst_idx, valves, vec![false; valves.len()]).unwrap()
}

fn _shortest_distance(
    origin: &Valve,
    dst_idx: usize,
    valves: &Vec<Valve>,
    visited: Vec<bool>,
) -> Option<usize> {
    let destination = valves.iter().find(|v| v.id == dst_idx).unwrap();
    if *origin == *destination {
        return Some(0);
    }
    let distances = origin
        .tunnels
        .iter()
        .filter(|t| !visited[**t])
        .enumerate()
        .filter_map(|(i, t)| {
            let new_origin = valves.iter().find(|v| v.id == *t).unwrap();
            let mut visited = visited.clone();
            visited[*t] = true;
            match _shortest_distance(new_origin, dst_idx, valves, visited) {
                Some(d) => Some(d + origin.distances[i]),
                None => None,
            }
        })
        .collect::<Vec<usize>>();
    // HOrrible
    match distances.len() {
        0 => None,
        _ => distances.iter().min().copied(),
    }
}

fn simplify_graph(valves: &Vec<Valve>, important: Vec<Valve>) -> Vec<Valve> {
    let char_tunnels = important.iter().map(|v| v.char_id).collect::<Vec<Id>>();
    let tunnels = important.iter().map(|v| v.id).collect::<Vec<usize>>();
    let mut new_valves = important
        .iter()
        .map(|v| {
            let new_valve = Valve {
                id: v.id,
                char_id: v.char_id,
                flow_rate: v.flow_rate,
                char_tunnels: char_tunnels.clone(),
                tunnels: tunnels.clone(),
                distances: tunnels
                    .iter()
                    .map(|t| shortest_distance(v, *t, valves))
                    .collect(),
            };
            new_valve
        })
        .collect::<Vec<Valve>>();
    let id_map = new_valves
        .iter()
        .enumerate()
        .map(|(i, v)| (v.id, i))
        .collect::<Vec<(usize, usize)>>();
    new_valves.iter_mut().for_each(|v| {
        v.id = id_map.iter().find(|m| m.0 == v.id).unwrap().1;
        v.tunnels = v
            .tunnels
            .iter()
            .map(|t| id_map.iter().find(|m| m.0 == *t).unwrap().1)
            .collect()
    });
    new_valves
}

fn maximize_flow_rate(valves: &Vec<Valve>, timeout: usize) -> usize {
    let mut important = valves
        .iter()
        .filter(|v| v.flow_rate > 0)
        .cloned()
        .collect::<Vec<Valve>>();
    let start_valve = valves.iter().find(|v| v.char_id == Id('A', 'A')).unwrap();
    important.push(start_valve.clone());
    let valves = simplify_graph(valves, important);
    let start_valve = valves.iter().find(|v| v.char_id == Id('A', 'A')).unwrap();
    let mut open = vec![false; valves.len()];
    open[start_valve.id] = true;
    _maximize_flow_rate(start_valve.id, &valves, timeout, open).unwrap()
}
fn _maximize_flow_rate(
    src_idx: usize,
    valves: &Vec<Valve>,
    timeout: usize,
    open: Vec<bool>,
) -> Option<usize> {
    // 1. Open valve
    let mut open = open;
    open[src_idx] = true;
    let valve = valves[src_idx].clone();

    // 2. Accumulate flow
    let current_flow: usize = valves
        .iter()
        .enumerate()
        .filter(|(i, _)| open[*i])
        .map(|(_, v)| v.flow_rate)
        .sum();

    // 3. Guard clause
    if timeout == 0 || open.iter().all(|o| *o) {
        return Some(current_flow * timeout);
    }

    // 4. Find possible paths
    let flows = valves
        .iter()
        .enumerate()
        .filter(|(i, _)| !open[*i])
        .filter_map(|(i, v)| {
            if timeout < (1 + valve.distances[i]) {
                return Some(current_flow * timeout);
            }
            let next_time = timeout - 1 - valve.distances[i];
            match _maximize_flow_rate(v.id, valves, next_time, open.clone()) {
                Some(flow) => Some(flow + (valve.distances[i] + 1) * current_flow),
                None => None,
            }
        })
        .collect::<Vec<usize>>();

    // 5. Return minimum
    match flows.len() {
        0 => None,
        _ => flows.iter().max().copied(),
    }
}

fn maximize_flow_rate_with_elephant(valves: &Vec<Valve>, timeout: usize) -> usize {
    let mut important = valves
        .iter()
        .filter(|v| v.flow_rate > 0)
        .cloned()
        .collect::<Vec<Valve>>();
    let start_valve = valves.iter().find(|v| v.char_id == Id('A', 'A')).unwrap();
    important.push(start_valve.clone());
    let valves = simplify_graph(valves, important);
    let start_valve = valves.iter().find(|v| v.char_id == Id('A', 'A')).unwrap();
    let mut open = vec![false; valves.len()];
    open[start_valve.id] = true;
    dbg!(&valves);
    _maximize_flow_rate_with_elephant(start_valve.id, start_valve.id, 0, 0, &valves, timeout, open)
        .unwrap()
}

fn _maximize_flow_rate_with_elephant(
    me_idx: usize,
    ele_idx: usize,
    me_wait: usize,
    ele_wait: usize,
    valves: &Vec<Valve>,
    timeout: usize,
    open: Vec<bool>,
) -> Option<usize> {
    // // 1. Open valve
    // let mut open = open;
    // open[src_idx] = true;
    // let valve = valves[src_idx].clone();

    // // 2. Accumulate flow
    // let current_flow: usize = valves
    //     .iter()
    //     .enumerate()
    //     .filter(|(i, _)| open[*i])
    //     .map(|(_, v)| v.flow_rate)
    //     .sum();

    // // 3. Guard clause
    // if timeout == 0 || open.iter().all(|o| *o) {
    //     return Some(current_flow * timeout);
    // }

    // // 4. Find possible paths
    // let flows = valves
    //     .iter()
    //     .enumerate()
    //     .filter(|(i, _)| !open[*i])
    //     .filter_map(|(i, v)| {
    //         if timeout < (1 + valve.distances[i]) {
    //             return Some(current_flow * timeout);
    //         }
    //         let next_time = timeout - 1 - valve.distances[i];
    //         match _maximize_flow_rate_with_elephant(v.id, valves, next_time, open.clone()) {
    //             Some(flow) => Some(flow + (valve.distances[i] + 1) * current_flow),
    //             None => None,
    //         }
    //     })
    //     .collect::<Vec<usize>>();

    // // 5. Return minimum
    // match flows.len() {
    //     0 => None,
    //     _ => flows.iter().max().copied(),
    // }
    None
}

fn solve_pt1(input_text: &str) -> u64 {
    let valves = parse_input(input_text);
    let timeout = 30;
    maximize_flow_rate(&valves, timeout) as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    let valves = parse_input(input_text);
    let timeout = 26;
    maximize_flow_rate_with_elephant(&valves, timeout) as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_16_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // Correct: 1896

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // Correct: BBB

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II";
    test_solution!(test1, solve_pt1, 1651, TEST_DATA);
    test_solution!(test2, solve_pt2, 1707, TEST_DATA);
}
