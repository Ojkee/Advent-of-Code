use std::{cmp::max, fs};

#[allow(dead_code)]
pub fn part1() -> usize {
    let data = fs::read_to_string("data/day5.txt").unwrap();
    let (ranges, ingreds) = data.split_once("\n\n").unwrap();
    let ranges = ranges
        .split("\n")
        .map(|line| {
            let (a, b) = line.trim().split_once("-").unwrap();
            (a.to_string(), b.to_string())
        })
        .map(|(a, b)| (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap()))
        .collect::<Vec<_>>();
    ingreds
        .split("\n")
        .map(|line| line.trim())
        .map(|n| n.parse::<usize>().unwrap_or(0))
        .filter(|n| ranges.iter().any(|(a, b)| a <= n && n <= b))
        .collect::<Vec<_>>()
        .len()
}

#[allow(dead_code)]
pub fn part2() -> usize {
    let data = fs::read_to_string("data/day5.txt").unwrap();
    let (ranges, _) = data.split_once("\n\n").unwrap();
    let mut ranges = ranges
        .split("\n")
        .map(|line| {
            let (a, b) = line.trim().split_once("-").unwrap();
            (a.to_string(), b.to_string())
        })
        .map(|(a, b)| (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap()))
        .collect::<Vec<_>>();
    ranges.sort_unstable_by_key(|(a, _)| *a);
    ranges
        .iter()
        .fold(Vec::new(), |acc, interval| merge_intervals(acc, interval))
        .iter()
        .fold(0 as usize, |acc, (a, b)| acc + 1 + b - a)
}

fn merge_intervals(
    mut intervals: Vec<(usize, usize)>,
    (s, e): &(usize, usize),
) -> Vec<(usize, usize)> {
    if intervals.is_empty() {
        intervals.push((*s, *e));
        return intervals;
    }
    if let Some(last) = intervals.last_mut() {
        if *s <= last.1 {
            last.1 = max(last.1, *e);
        } else {
            intervals.push((*s, *e));
        }
    }
    intervals
}
