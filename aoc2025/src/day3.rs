use std::fs;

#[allow(dead_code)]
pub fn part1() -> u32 {
    fs::read_to_string("data/day3.txt")
        .unwrap()
        .split("\n")
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(calc_joltage_2)
        .sum()
}

fn calc_joltage_2(bank: &str) -> u32 {
    let battery = bank.chars().collect::<Vec<_>>();
    let (lhs_idx, lhs) = max_in_range(&battery, 0, battery.len() - 1);
    let (_, rhs) = max_in_range(&battery, lhs_idx + 1, battery.len());

    10 * lhs + rhs
}

fn max_in_range(battery: &[char], s: usize, e: usize) -> (usize, u32) {
    let mut idx = s;
    for i in s..e {
        if battery.get(i).unwrap() > battery.get(idx).unwrap() {
            idx = i;
        }
    }
    (idx, battery[idx].to_digit(10).unwrap())
}

#[allow(dead_code)]
pub fn part2() -> usize {
    fs::read_to_string("data/day3.txt")
        .unwrap()
        .split("\n")
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(calc_joltage_12)
        .sum::<usize>()
}

fn calc_joltage_12(bank: &str) -> usize {
    let mut battery = bank
        .chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
        .collect::<Vec<_>>();
    let n_remove = battery.len() - 12;
    for _ in 0..n_remove {
        let remove_idx = first_increasing_idx(&battery);
        battery.remove(remove_idx);
    }
    battery
        .iter()
        .fold(0usize, |acc, n| (acc * 10 + n) as usize)
}

fn first_increasing_idx(battery: &[usize]) -> usize {
    for i in 0..(battery.len() - 1) {
        let current = battery[i];
        let next = battery[i + 1];
        if current < next {
            return i;
        }
    }
    battery.len() - 1
}
