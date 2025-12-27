use std::fs;

#[allow(dead_code)]
pub fn part1() -> usize {
    let diagram = fs::read_to_string("data/day4.txt")
        .unwrap()
        .split("\n")
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    count_accesable(&diagram)
}

fn count_accesable(diagram: &[Vec<char>]) -> usize {
    let mut count = 0;
    for i in 0..diagram.len() {
        for j in 0..diagram[i].len() {
            if valid_surrounding(diagram, i, j) {
                count += 1;
            }
        }
    }
    count
}

fn valid_surrounding(diagram: &[Vec<char>], y: usize, x: usize) -> bool {
    if diagram[y][x] != '@' {
        return false;
    };
    let dirs = vec![
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let mut count = 0;
    let rows = diagram.len() as i32;
    let cols = diagram[0].len() as i32;
    for (dy, dx) in dirs {
        let yy = y as i32 + dy;
        let xx = x as i32 + dx;
        if yy < 0 || rows <= yy || xx < 0 || cols <= xx {
            continue;
        }
        if diagram[yy as usize][xx as usize] == '@' {
            count += 1;
        }
    }
    count < 4
}

#[allow(dead_code)]
pub fn part2() -> usize {
    let mut diagram = fs::read_to_string("data/day4.txt")
        .unwrap()
        .split("\n")
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut res = 0;
    loop {
        let to_remove = collect_to_remove(&diagram);
        if to_remove.is_empty() {
            break;
        }
        res += to_remove.len();
        remove_rolls(&mut diagram, &to_remove);
    }
    res
}

fn collect_to_remove(diagram: &[Vec<char>]) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    for (i, row) in diagram.iter().enumerate() {
        for j in 0..row.len() {
            if valid_surrounding(&diagram, i, j) {
                res.push((i, j));
            }
        }
    }
    res
}

fn remove_rolls(diagram: &mut [Vec<char>], to_remove: &[(usize, usize)]) {
    for (y, x) in to_remove {
        diagram[*y][*x] = '.';
    }
}
