use std::fs;

#[allow(dead_code)]
pub fn part1() -> u128 {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/data/day2.txt");
    fs::read_to_string(path)
        .unwrap()
        .split(",")
        .map(invalid_ids_sum_in_range)
        .sum()
}

fn invalid_ids_sum_in_range(range: &str) -> u128 {
    let parts: Vec<_> = range.split("-").collect();
    let [a, b] = parts[..] else {
        panic!("Invalid range");
    };
    let a = a.trim().parse::<u128>().unwrap_or(0);
    let b = b.trim().parse::<u128>().unwrap_or(0);
    let mut res: u128 = 0;
    for num in a..=b {
        let n = num.to_string();
        res += match n.split_at(n.len() / 2) {
            (lhs, rhs) if lhs == rhs => num,
            _ => 0,
        }
    }
    res
}

#[allow(dead_code)]
pub fn part2() -> u128 {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/data/day2.txt");
    fs::read_to_string(path)
        .unwrap()
        .split(",")
        .map(invalid_ids_sum_in_range_p2)
        .sum()
}

fn invalid_ids_sum_in_range_p2(range: &str) -> u128 {
    let parts: Vec<_> = range.split("-").collect();
    let [a, b] = parts[..] else {
        panic!("Invalid range");
    };
    let a = a.trim().parse::<u128>().unwrap_or(0);
    let b = b.trim().parse::<u128>().unwrap_or(0);
    let mut res: u128 = 0;
    for num in a..=b {
        let n = num.to_string();
        for i in 1..=(n.len() / 2) {
            if !n.len().is_multiple_of(i) {
                continue;
            }
            if all_eq(&chunk_of(&n, i)) {
                res += num;
                break;
            }
        }
    }

    res
}

fn all_eq(c: &[&str]) -> bool {
    c.first().map(|f| c.iter().all(|x| x == f)).unwrap_or(false)
}

fn chunk_of(s: &str, size: usize) -> Vec<&str> {
    s.as_bytes()
        .chunks(size)
        .map(|chunk| std::str::from_utf8(chunk).unwrap())
        .collect::<Vec<&str>>()
}
