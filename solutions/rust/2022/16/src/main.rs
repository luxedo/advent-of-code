/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 16
* Rust Solution
*
* Day 16: Proboscidea Volcanium
*
* https://adventofcode.com/2022/day/16
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
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

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let valves = parse_input(input_data);
    let timeout = 30;
    Ok(maximize_flow_rate(&valves, timeout))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let valves = parse_input(input_data);
    let timeout = 26;
    Ok(maximize_flow_rate_with_elephant(&valves, timeout))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
