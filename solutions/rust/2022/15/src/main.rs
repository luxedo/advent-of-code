/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 15
* Rust Solution
*
* Day 15: Beacon Exclusion Zone
*
* https://adventofcode.com/2022/day/15
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::collections::BTreeSet;
use std::fmt;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
struct Coord {
    x: isize,
    y: isize,
}
impl Coord {
    fn manhattan(&self, other: &Self) -> isize {
        (self.x.abs_diff(other.x) + self.y.abs_diff(other.y)) as isize
    }
}

#[derive(Debug, Clone, Copy)]
struct Scan {
    beacon: Coord,
    sensor: Coord,
    distance: isize,
}
impl Scan {
    fn in_range(&self, coord: &Coord) -> bool {
        self.sensor.manhattan(&coord) <= self.distance
    }
    fn is_beacon(&self, coord: &Coord) -> bool {
        self.beacon == *coord
    }
    fn is_sensor(&self, coord: &Coord) -> bool {
        self.sensor == *coord
    }
}

struct Zone {
    scans: Vec<Scan>,
    x_min: isize,
    x_max: isize,
    y_min: isize,
    y_max: isize,
}
impl fmt::Debug for Zone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let zone_str = (self.y_min..self.y_max)
            .map(|y| {
                format!("{:03}", y.to_string())
                    + &(self.x_min..=self.x_max)
                        .map(|x| {
                            let c = &Coord { x, y };
                            self.scans.iter().fold('.', |acc, scan| {
                                if acc != '.' && acc != '#' {
                                    return acc;
                                } else if scan.is_beacon(c) {
                                    return 'B';
                                } else if scan.is_sensor(c) {
                                    return 'S';
                                } else if scan.in_range(c) {
                                    return '#';
                                } else {
                                    return acc;
                                }
                            })
                        })
                        .collect::<String>()
                    + "\n"
            })
            .collect::<String>();
        write!(f, "\n{}", zone_str)
    }
}
impl Zone {
    fn from_string(input: &str) -> Self {
        let scans = input
            .lines()
            .map(|line| {
                let (sensor, beacon) = line.split_once(":").unwrap();
                let (sensor, sy) = sensor.split_once(", y=").unwrap();
                let (beacon, by) = beacon.split_once(", y=").unwrap();
                let (_, sx) = sensor.split_once("x=").unwrap();
                let (_, bx) = beacon.split_once("x=").unwrap();
                let sx = sx.parse::<isize>().unwrap();
                let bx = bx.parse::<isize>().unwrap();
                let sy = sy.parse::<isize>().unwrap();
                let by = by.parse::<isize>().unwrap();
                let sensor = Coord { x: sx, y: sy };
                let beacon = Coord { x: bx, y: by };
                let distance = sensor.manhattan(&beacon);
                Scan {
                    sensor,
                    beacon,
                    distance,
                }
            })
            .collect::<Vec<Scan>>();
        let x_min = scans
            .iter()
            .map(|scan| scan.sensor.x - scan.distance)
            .min()
            .unwrap();
        let x_max = scans
            .iter()
            .map(|scan| scan.sensor.x + scan.distance)
            .max()
            .unwrap();
        let y_min = scans
            .iter()
            .map(|scan| scan.sensor.y - scan.distance)
            .min()
            .unwrap();
        let y_max = scans
            .iter()
            .map(|scan| scan.sensor.y + scan.distance)
            .max()
            .unwrap();

        Self {
            scans,
            x_min,
            x_max,
            y_min,
            y_max,
        }
    }
    fn not_beacon(&self, c: &Coord) -> bool {
        self.scans
            .iter()
            .map(|s| s.in_range(c) && !s.is_beacon(c))
            .any(|in_range| in_range)
    }
    fn not_beacon_row(&self, y: isize) -> isize {
        (self.x_min..self.x_max)
            .map(|x| self.not_beacon(&Coord { x, y }) as isize)
            .sum::<isize>()
    }
    fn is_empty(&self, c: &Coord) -> bool {
        self.scans
            .iter()
            .map(|s| !s.in_range(c))
            .all(|not_in_range| not_in_range)
    }
    fn distress_beacon(&self, low: isize, high: isize) -> Option<Coord> {
        let candidates = self
            .scans
            .iter()
            .flat_map(|scan| {
                (0..=(scan.distance + 1))
                    .flat_map(|i| {
                        let j = (scan.distance + 1) - i;
                        let Coord { x, y } = scan.sensor;
                        vec![
                            Coord { x: x + i, y: y + j },
                            Coord { x: x + i, y: y - j },
                            Coord { x: x - i, y: y - j },
                            Coord { x: x - i, y: y + j },
                        ]
                    })
                    .filter(|c| {
                        c.x >= low && c.x <= high && c.y >= low && c.y <= high && self.is_empty(c)
                    })
                    .collect::<BTreeSet<Coord>>()
            })
            .collect::<BTreeSet<Coord>>();
        candidates.iter().next().copied()
    }
}

fn solve_pt1(input_data: &str, args: Vec<String>) -> FireplaceResult<impl Display> {
    let zone = Zone::from_string(input_data);
    let y = match args.len() {
        0 => 2000000,
        1 => args[0].parse::<isize>().unwrap(),
        _ => panic!("Wait what?"),
    };
    Ok(zone.not_beacon_row(y))
}

fn solve_pt2(input_data: &str, args: Vec<String>) -> FireplaceResult<impl Display> {
    const X_MUL: isize = 4000000;
    let (low, high) = match args.len() {
        0 => (0, 4000000),
        2 => (args[0].parse::<isize>().unwrap(), args[1].parse::<isize>().unwrap()),
        _ => panic!("Wait what?"),
    };
    let zone = Zone::from_string(input_data);
    let distress = zone.distress_beacon(low, high).unwrap();
    Ok(X_MUL * distress.x + distress.y)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
