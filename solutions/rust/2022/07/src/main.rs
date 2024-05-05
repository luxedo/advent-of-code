/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 07
* Rust Solution
*
* Day 7: No Space Left On Device
*
* https://adventofcode.com/2022/day/7
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

const SHELL_ANCHOR: &str = "$";
const LS: &str = "ls";
const CD: &str = "cd";
const DIR_ANCHOR: &str = "dir";
const PARENT_DIR_ANCHOR: &str = "..";
const ROOT_DIR_ANCHOR: &str = "/";

#[derive(Debug)]
struct Root {
    dirs: Vec<Dir>,
    root_idx: usize,
    cwd_idx: usize,
}

impl Root {
    fn new() -> Self {
        let root_dir = Dir::new(ROOT_DIR_ANCHOR, 0);
        Root {
            dirs: vec![root_dir],
            root_idx: 0,
            cwd_idx: 0,
        }
    }

    fn new_file(&mut self, name: &str, size: usize) {
        self.get_cwd().files.push(File {
            name: name.to_string(),
            size: size,
        });
    }
    fn new_dir(&mut self, name: &str) {
        let index = self.dirs.len();
        let new_dir = Dir::new(name, self.cwd_idx);
        self.dirs.push(new_dir);
        self.get_cwd().dirs.push(index);
    }
    fn get_cwd(&mut self) -> &mut Dir {
        &mut self.dirs[self.cwd_idx]
    }
    fn get_root_dir(&mut self) -> &mut Dir {
        &mut self.dirs[self.root_idx]
    }
    fn get_dir_by_index(&mut self, index: usize) -> &mut Dir {
        &mut self.dirs[index]
    }
    fn change_dir(&mut self, name: &str) {
        match name {
            ROOT_DIR_ANCHOR => self.cwd_idx = self.root_idx,
            PARENT_DIR_ANCHOR => self.cwd_idx = self.get_cwd().parent,
            _ => {
                self.cwd_idx = self.find_dir_index(name);
            }
        }
    }
    fn find_dir_index(&mut self, name: &str) -> usize {
        let cwd = self.get_cwd().clone();
        *cwd.dirs
            .iter()
            .find(|i| self.get_dir_by_index(**i).name == name)
            .unwrap()
    }
    fn compute_dir_sizes(&mut self) {
        for (i, dir) in self.dirs.clone().iter_mut().enumerate().rev() {
            let file_sizes = dir.files.iter_mut().fold(0, |acc, f| acc + f.size);
            let dir_sizes: usize = dir
                .dirs
                .clone()
                .into_iter()
                .map(|j| self.dirs[j].size)
                .sum();
            self.dirs[i].size = file_sizes + dir_sizes;
        }
    }
    fn list_dir_sizes(&mut self) -> Vec<u64> {
        let mut sizes = self
            .dirs
            .iter_mut()
            .map(|d| d.size as u64)
            .collect::<Vec<u64>>();
        sizes.sort();
        sizes
    }
}

#[derive(Debug, Clone)]
struct Dir {
    name: String,
    parent: usize,
    dirs: Vec<usize>,
    files: Vec<File>,
    size: usize,
}

impl Dir {
    fn new(name: &str, parent: usize) -> Self {
        Dir {
            name: name.to_string(),
            parent: parent,
            dirs: vec![],
            files: vec![],
            size: 0,
        }
    }
}

#[derive(Debug, Clone)]
struct File {
    name: String,
    size: usize,
}

fn parse_dir_structure(commands: &mut Vec<&str>, root: &mut Root) {
    match commands.len() {
        0 => (),
        _ => {
            let shell_interaction = commands.remove(0);
            let (full_command, output) = shell_interaction
                .split_once("\n")
                .unwrap_or((shell_interaction, ""));
            let (cmd_str, args) = full_command.split_once(" ").unwrap_or((full_command, ""));
            match cmd_str {
                CD => {
                    root.change_dir(args);
                }
                LS => output.split("\n").for_each(|row| {
                    let (head, tail) = row.split_once(" ").unwrap();
                    match head {
                        DIR_ANCHOR => {
                            root.new_dir(tail);
                        }
                        _ => {
                            root.new_file(tail, head.parse::<usize>().unwrap());
                        }
                    };
                }),
                &_ => panic!("Command not found"),
            };
            parse_dir_structure(commands, root);
        }
    }
}

fn input_to_root(input_text: &str, root: &mut Root) {
    let mut commands = input_text
        .strip_prefix(SHELL_ANCHOR)
        .unwrap()
        .split(SHELL_ANCHOR)
        .map(|cmd| cmd.trim())
        .collect::<Vec<_>>();

    parse_dir_structure(&mut commands, root);
    root.compute_dir_sizes();
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let at_most_pt1: u64 = 100000;
    let mut root = Root::new();
    input_to_root(input_data, &mut root);
    let answer = root.list_dir_sizes()
        .into_iter()
        .filter(|a| a < &at_most_pt1)
        .sum::<u64>();
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let total_disk: u64 = 70000000;
    let at_least: u64 = 30000000;
    let mut root = Root::new();
    input_to_root(input_data, &mut root);
    let root_size = root.get_root_dir().size as u64;
    let free_at_most = at_least - (total_disk - root_size);
    let answer = root.list_dir_sizes()
        .into_iter()
        .find(|a| a > &free_at_most && a != &root_size)
        .unwrap();
    Ok(answer)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
