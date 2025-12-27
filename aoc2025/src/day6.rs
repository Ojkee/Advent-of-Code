use std::fs;

pub fn part1() -> usize {
    let content = fs::read_to_string("data/day6.txt").unwrap();
    let rows = content
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| line.split_whitespace().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let transposed = (0..rows[0].len()).map(|i| rows.iter().map(|row| row[i]).collect::<Vec<_>>());
    transposed.map(sum_line).sum::<usize>()
}

fn sum_line(mut line: Vec<&str>) -> usize {
    line.reverse();
    let parse_to_nums =
        |slc: &[&str]| -> Vec<usize> { slc.iter().map(|n| n.parse::<usize>().unwrap()).collect() };
    match line.as_slice() {
        ["*", rest @ ..] => parse_to_nums(rest).iter().product(),
        ["+", rest @ ..] => parse_to_nums(rest).iter().sum(),
        _ => unreachable!(),
    }
}

// 3263827
pub fn part2() -> usize {
    let _ = fs::read_to_string("data/day6.txt").unwrap();
    0
}
