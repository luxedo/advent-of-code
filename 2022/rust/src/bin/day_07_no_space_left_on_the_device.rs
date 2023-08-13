/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/7
 *
 * --- Day 7: No Space Left On Device ---
 * You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear much louder sounds in the distance; how big do the animals get out here, anyway?
 *
 * The device the Elves gave you has problems with more than just its communication system. You try to run a system update:
 *
 * $ system-update --please --pretty-please-with-sugar-on-top
 * Error: No space left on device
 * Perhaps you can delete some files to make space for the update?
 *
 * You browse around the filesystem to assess the situation and save the resulting terminal output (your puzzle input). For example:
 *
 * $ cd /
 * $ ls
 * dir a
 * 14848514 b.txt
 * 8504156 c.dat
 * dir d
 * $ cd a
 * $ ls
 * dir e
 * 29116 f
 * 2557 g
 * 62596 h.lst
 * $ cd e
 * $ ls
 * 584 i
 * $ cd ..
 * $ cd ..
 * $ cd d
 * $ ls
 * 4060174 j
 * 8033020 d.log
 * 5626152 d.ext
 * 7214296 k
 * The filesystem consists of a tree of files (plain data) and directories (which can contain other directories or files). The outermost directory is called /. You can navigate around the filesystem, moving into or out of directories and listing the contents of the directory you're currently in.
 *
 * Within the terminal output, lines that begin with $ are commands you executed, very much like some modern computers:
 *
 * cd means change directory. This changes which directory is the current directory, but the specific result depends on the argument:
 * cd x moves in one level: it looks in the current directory for the directory named x and makes it the current directory.
 * cd .. moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
 * cd / switches the current directory to the outermost directory, /.
 * ls means list. It prints out all of the files and directories immediately contained by the current directory:
 * 123 abc means that the current directory contains a file named abc with size 123.
 * dir xyz means that the current directory contains a directory named xyz.
 * Given the commands and output in the example above, you can determine that the filesystem looks visually like this:
 *
 * - / (dir)
 *   - a (dir)
 *     - e (dir)
 *       - i (file, size=584)
 *     - f (file, size=29116)
 *     - g (file, size=2557)
 *     - h.lst (file, size=62596)
 *   - b.txt (file, size=14848514)
 *   - c.dat (file, size=8504156)
 *   - d (dir)
 *     - j (file, size=4060174)
 *     - d.log (file, size=8033020)
 *     - d.ext (file, size=5626152)
 *     - k (file, size=7214296)
 * Here, there are four directories: / (the outermost directory), a and d (which are in /), and e (which is in a). These directories also contain files of various sizes.
 *
 * Since the disk is full, your first step should probably be to find directories that are good candidates for deletion. To do this, you need to determine the total size of each directory. The total size of a directory is the sum of the sizes of the files it contains, directly or indirectly. (Directories themselves do not count as having any intrinsic size.)
 *
 * The total sizes of the directories above can be found as follows:
 *
 * The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.
 * The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).
 * Directory d has total size 24933642.
 * As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.
 * To begin, find all of the directories with a total size of at most 100000, then calculate the sum of their total sizes. In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584). (As in this example, this process can count files more than once!)
 *
 * Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?
 *
 * --- Part Two ---
 * Now, you're ready to choose a directory to delete.
 *
 * The total disk space available to the filesystem is 70000000. To run the update, you need unused space of at least 30000000. You need to find a directory you can delete that will free up enough space to run the update.
 *
 * In the example above, the total size of the outermost directory (and thus the total amount of used space) is 48381165; this means that the size of the unused space must currently be 21618835, which isn't quite the 30000000 required by the update. Therefore, the update still requires a directory with total size of at least 8381165 to be deleted before it can run.
 *
 * To achieve this, you have the following options:
 *
 * Delete directory e, which would increase unused space by 584.
 * Delete directory a, which would increase unused space by 94853.
 * Delete directory d, which would increase unused space by 24933642.
 * Delete directory /, which would increase unused space by 48381165.
 * Directories e and a are both too small; deleting them would not free up enough space. However, directories d and / are both big enough! Between these, choose the smallest: d, increasing unused space by 24933642.
 *
 * Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update. What is the total size of that directory?
 */

use aoc_rust::load_input;
use std::error::Error;

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

fn solve_pt1(input_text: &str) -> u64 {
    let at_most_pt1: u64 = 100000;
    let mut root = Root::new();
    input_to_root(input_text, &mut root);
    root.list_dir_sizes()
        .into_iter()
        .filter(|a| a < &at_most_pt1)
        .sum::<u64>()
}

fn solve_pt2(input_text: &str) -> u64 {
    let total_disk: u64 = 70000000;
    let at_least: u64 = 30000000;
    let mut root = Root::new();
    input_to_root(input_text, &mut root);
    let root_size = root.get_root_dir().size as u64;
    let free_at_most = at_least - (total_disk - root_size);
    root.list_dir_sizes()
        .into_iter()
        .find(|a| a > &free_at_most && a != &root_size)
        .unwrap()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_07_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 1206825

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 9608311

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {

    use aoc_rust::test_solution;

    const TEST_DATA: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    test_solution!(test1, solve_pt1, 95437, TEST_DATA);
    test_solution!(test2, solve_pt2, 24933642, TEST_DATA);
}
