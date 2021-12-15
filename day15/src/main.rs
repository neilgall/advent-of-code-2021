use std::cmp::min;
use std::collections::HashSet;

// -- model

type Dimension = i32;
type Coordinate = i32;
type Risk = u32;

#[derive(Clone,Debug)]
struct Map<T> {
    cells: Vec<Vec<T>>,
    pub height: Dimension,
    pub width: Dimension
}

#[derive(Copy,Clone,Debug,Eq,Hash,PartialEq)]
struct Pos {
    pub x: Coordinate,
    pub y: Coordinate
}

impl<T: Copy> Map<T> {
    fn new(width: Dimension, height: Dimension, default: T) -> Self {
        let mut cells: Vec<Vec<T>> = Vec::with_capacity(height as usize);
        for _ in 0..height {
            let mut row: Vec<T> = Vec::with_capacity(width as usize);
            for _ in 0..width {
                row.push(default);
            }
            cells.push(row);
        }
        Map { cells, width, height }
    }

    fn contains(&self, p: &Pos) -> bool {
        0 <= p.x && 0 <= p.y && p.x < self.width && p.y < self.height
    }

    fn at(&self, p: &Pos) -> T {
        assert!(self.contains(p));
        self.cells[p.y as usize][p.x as usize]
    }

    fn set_at(&mut self, p: &Pos, value: T) {
        assert!(self.contains(p));
        self.cells[p.y as usize][p.x as usize] = value;
    }

    fn iter(&self) -> impl Iterator<Item = Pos> + '_ {
        (0..self.height).flat_map(move |y|
            (0..self.width).map(move |x| Pos { x, y })
        )
    }
}

impl Map<Risk> {
    fn parse(input: &str) -> Self {
        let cells: Vec<Vec<Risk>> = input.lines().map(|line|
            line.chars().map(|c|
                (c as Risk) - ('0' as Risk)
            ).collect()
        ).collect();
        let height = cells.len() as Dimension;
        let width = cells.iter().map(|row| row.len()).max().unwrap() as Dimension;
        Map { cells, width, height }
    }
}


impl Pos {
    fn up(&self) -> Pos {
        Pos { x: self.x, y: self.y - 1 }
    }
    
    fn down(&self) -> Pos {
        Pos { x: self.x, y: self.y + 1 }
    }
    
    fn left(&self) -> Pos {
        Pos { x: self.x - 1, y: self.y }
    }

    fn right(&self) -> Pos {
        Pos { x: self.x + 1, y: self.y }
    }

    fn neighbours(&self) -> impl Iterator<Item = Pos> + '_ {
        vec![self.up(), self.down(), self.left(), self.right()].into_iter()
    }
}

fn pos(x: Coordinate, y: Coordinate) -> Pos {
    Pos { x, y }
}

// -- problems

fn dijkstra(map: &Map<Risk>, from: Pos, to: Pos) -> Risk {
    let mut risks: Map<Risk> = Map::new(map.width, map.height, Risk::max_value());
    let mut unvisited: HashSet<Pos> = risks.iter().collect();
    risks.set_at(&from, 0);

    let mut current = from;
    while current != to && unvisited.contains(&current) {
        unvisited.remove(&current);
        let current_risk = risks.at(&current);

        let neighbours: Vec<Pos> = current.neighbours()
            .filter(|n| map.contains(n) && unvisited.contains(n))
            .collect();

        neighbours.iter().for_each(|n| {
            let total_risk = current_risk + map.at(&n);
            risks.set_at(&n, min(total_risk, risks.at(&n)));
        });
        
        current = *unvisited.iter()
            .map(|p| (p, risks.at(&p)))
            .min_by(|a, b| a.1.cmp(&b.1))
            .map(|(p, _)| p)
            .unwrap();
    }

    risks.at(&to)
}

fn part1(map: &Map<Risk>) -> Risk {
    let start = pos(0, 0);
    let end = pos(map.width-1, map.height-1);
    dijkstra(map, start, end)
}

fn main() {
    let input = std::fs::read_to_string("./input.txt")
        .expect("can't load input.txt");
    let map = Map::parse(&input);

    println!("Part 1: {:?}", part1(&map));
}


#[cfg(test)]
mod tests {
    use super::*;

    fn example_input() -> &'static str {
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"
    }

    #[test]
    fn test_new_map() {
        let map = Map::parse(example_input());
        assert_eq!(map.height, 10);
        assert_eq!(map.width, 10);
        assert_eq!(map.at(&pos(0, 0)), 1);
        assert_eq!(map.at(&pos(9, 0)), 2);
        assert_eq!(map.at(&pos(0, 9)), 2);
        assert_eq!(map.at(&pos(9, 9)), 1);
    }

    #[test]
    fn test_bounds_checks() {
        let map = Map::parse(example_input());
        assert_eq!(map.contains(&pos(0, 0)), true);
        assert_eq!(map.contains(&pos(9, 9)), true);
        assert_eq!(map.contains(&pos(-1, 0)), false);
        assert_eq!(map.contains(&pos(0, -1)), false);
        assert_eq!(map.contains(&pos(10, 0)), false);
        assert_eq!(map.contains(&pos(0, 10)), false);
    }

    #[test]
    fn test_part1() {
        let map = Map::parse(example_input());
        assert_eq!(part1(&map), 40);
    }

}