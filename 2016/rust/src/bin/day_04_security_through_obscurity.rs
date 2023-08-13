/* Advent Of Code 2016 - day 4
 * https://adventofcode.com/2016/day/4
 *
 * --- Day 4: Security Through Obscurity ---
 * Finally, you come across an information kiosk with a list of rooms.  Of course, the list is encrypted and full of decoy data, but the instructions to decode the list are barely hidden nearby.  Better remove the decoy data first.
 *
 * Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.
 *
 * A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization.  For example:
 *
 * aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
 * a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
 * not-a-real-room-404[oarel] is a real room.
 * totally-real-room-200[decoy] is not.
 *
 *
 * Of the real rooms from the list above, the sum of their sector IDs is 1514.
 *
 * What is the sum of the sector IDs of the real rooms?
 *
 *
 * --- Part Two ---
 * With all the decoy data out of the way, it's time to decrypt this list and get moving.
 *
 * The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.
 *
 * To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.
 *
 * For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
 *
 * What is the sector ID of the room where North Pole objects are stored?
*/
#![allow(dead_code)]

use aoc_rust::load_input;
use std::collections::HashMap;
use std::error::Error;

#[derive(Debug)]
struct Room {
    name: String,
    sector_id: u64,
    checksum: String,
}

struct RoomParseError;
struct RoomDecryptError;
impl Room {
    fn new(name: String, sector_id: u64, checksum: String) -> Result<Self, RoomParseError> {
        if Self::calculate_checksum(&name) != checksum {
            Err(RoomParseError)
        } else {
            Ok(Self {
                name,
                sector_id,
                checksum,
            })
        }
    }
    fn parse(value: &str) -> Result<Self, RoomParseError> {
        let Some((head, tail)) = value.split_once('[') else {return Err(RoomParseError)};
        let checksum = tail[0..tail.len() - 1].to_string();
        let Some((name, sector_id)) = head.rsplit_once('-') else {return Err(RoomParseError)};
        let name = name.replace('-', " ");
        let sector_id = sector_id.parse::<u64>().unwrap();
        Self::new(name, sector_id, checksum)
    }
    fn calculate_checksum(name: &str) -> String {
        let mut counter = HashMap::new();
        name.chars()
            .filter(|letter| *letter != ' ')
            .for_each(|letter| *counter.entry(letter).or_insert(0) += 1);
        let mut counter = Vec::from_iter(counter.iter());
        counter.sort_by_key(|k| (-k.1, k.0));
        counter.truncate(5);
        counter.into_iter().map(|(l, _)| l).collect()
    }
    fn decrypt(&self) -> String {
        self.name
            .as_bytes()
            .iter()
            .map(|c| {
                if *c != b' ' {
                    char::from_u32(
                        b'a' as u32 + ((*c as u32 - b'a' as u32) + self.sector_id as u32) % 26,
                    )
                    .unwrap()
                } else {
                    ' '
                }
            })
            .collect()
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    input_text
        .lines()
        .filter_map(|line| Room::parse(line).ok())
        .map(|room| room.sector_id)
        .sum()
}

fn solve_pt2(input_text: &str, overwrite_sector_name: Option<&str>) -> u64 {
    let sector_name = overwrite_sector_name.unwrap_or("northpole object storage");
    input_text
        .lines()
        .filter_map(|line| Room::parse(line).ok())
        .find(|room| room.decrypt() == sector_name)
        .unwrap()
        .sector_id
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_04_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: 185371

    println!("Part two: {:#?}", solve_pt2(&input_text, None));
    // solution_pt2: 984

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    test_solution!(
        test1,
        solve_pt1,
        1514,
        "aaaaa-bbb-z-y-x-123[abxyz]\n\
    a-b-c-d-e-f-g-h-987[abcde]\n\
    not-a-real-room-404[oarel]\n\
    totally-real-room-200[decoy]"
    );
    test_solution!(
        test2,
        solve_pt2,
        343,
        "qzmt-zixmtkozy-ivhz-343[zimth]",
        Some("very encrypted name")
    );
}
