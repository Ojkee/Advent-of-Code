use std::env;
use std::fs;

#[allow(dead_code)]
pub fn part1() -> i32 {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/data/day1.txt");
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .fold((50, 0), |(acc, count), line| {
            count_zero_roll(acc, count, line)
        })
        .1
}

fn count_zero_roll(acc: i32, count: i32, line: &str) -> (i32, i32) {
    if line.is_empty() {
        return (acc, count);
    }
    let (dir, rest) = line.split_at(1);
    let num = rest.parse().unwrap_or(0);
    let delta = match dir {
        "L" => num,
        "R" => -num,
        _ => 0,
    };
    let rolled = acc + delta;
    (rolled, count + (rolled % 100 == 0) as i32)
}

#[allow(dead_code)]
pub fn part2() -> i32 {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/data/day1.txt");
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .fold((50, 0), |(acc, count), line| {
            count_zero_pass(acc, count, line)
        })
        .1
}

fn count_zero_pass(acc: i32, count: i32, line: &str) -> (i32, i32) {
    if line.is_empty() {
        return (acc, count);
    }
    let (dir, rest) = line.split_at(1);
    let num = rest.parse().unwrap_or(0);
    let delta: i32 = match dir {
        "L" => num,
        "R" => -num,
        _ => 0,
    };
    let passes = (delta / 100).abs();
    let left = delta - (delta / 100) * 100;
    let new_acc = acc + left;
    let carry = if (left > 0 && new_acc > 99) || (left < 0 && new_acc <= 0 && acc != 0) {
        1
    } else {
        0
    };
    ((new_acc + 100) % 100, count + passes + carry)
}
