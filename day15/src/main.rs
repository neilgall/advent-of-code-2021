use std::cmp::min;
use std::collections::HashSet;
use std::ops::{Index,IndexMut};

// -- model

type Dimension = i32;
type Coordinate = i32;
type Risk = u32;

#[derive(Copy,Clone,Debug,Eq,Hash,PartialEq)]
struct Pos {
    pub x: Coordinate,
    pub y: Coordinate
}

#[derive(Clone,Debug)]
struct Map<T> {
    cells: Vec<Vec<T>>,
    height: Dimension,
    width: Dimension
}

#[derive(Clone,Debug)]
struct TiledMap {
    cells: Vec<Vec<Risk>>,
    multiplier: Dimension,
    height: Dimension,
    width: Dimension
}

impl Pos {
    fn neighbours(&self) -> impl Iterator<Item = Pos> + '_ {
        vec![
            Pos { x: self.x, y: self.y - 1 },
            Pos { x: self.x, y: self.y + 1 },
            Pos { x: self.x - 1, y: self.y },
            Pos { x: self.x + 1, y: self.y }
        ].into_iter()
    }
}

impl<T: Clone> Map<T> {
    fn new(width: Dimension, height: Dimension, default: T) -> Self {
        let row: Vec<T> = vec![default; width as usize];
        let cells: Vec<Vec<T>> = vec![row; height as usize];
        Map { cells, width, height }
    }

    fn contains(&self, p: &Pos) -> bool {
        0 <= p.x && 0 <= p.y && p.x < self.width && p.y < self.height
    }    

    fn iter(&self) -> impl Iterator<Item = Pos> + '_ {
        (0..self.height).flat_map(move |y|
            (0..self.width).map(move |x| Pos { x, y })
        )
    }
}

impl<T: Clone> Index<&Pos> for Map<T> {
    type Output = T;

    fn index(&self, p: &Pos) -> &Self::Output {
        assert!(self.contains(p));
        &self.cells[p.y as usize][p.x as usize]
    }
}

impl<T: Clone> IndexMut<&Pos> for Map<T> {
    fn index_mut(&mut self, p: &Pos) -> &mut Self::Output {
        assert!(self.contains(p));
        &mut self.cells[p.y as usize][p.x as usize]
    }
}

impl TiledMap {
    fn parse(input: &str, multiplier: Dimension) -> Self {
        let cells: Vec<Vec<Risk>> = input.lines().map(|line|
            line.chars().map(|c|
                (c as Risk) - ('0' as Risk)
            ).collect()
        ).collect();
        let height = cells.len() as Dimension;
        let width = cells.iter().map(|row| row.len()).max().unwrap() as Dimension;
        TiledMap { cells, width, height, multiplier }
    }

    fn effective_width(&self) -> Dimension {
        self.width * self.multiplier
    }

    fn effective_height(&self) -> Dimension {
        self.height * self.multiplier
    }

    fn contains(&self, p: &Pos) -> bool {
        0 <= p.x 
            && 0 <= p.y
            && p.x < self.effective_width()
            && p.y < self.effective_height()
    }

    fn iter(&self) -> impl Iterator<Item = Pos> + '_ {
        (0..self.effective_height()).flat_map(move |y|
            (0..self.effective_width()).map(move |x| Pos { x, y })
        )
    }

    fn at(&self, p: &Pos) -> Risk {
        assert!(self.contains(p));
        let r = self.cells[(p.y % self.height) as usize][(p.x % self.width) as usize]
                + (p.x / self.width) as Risk
                + (p.y / self.height) as Risk;
        if self.multiplier == 1 {
            r
        } else if r % 9 == 0 { 
            9
        } else { 
            r % 9 
        }
    }
}

fn pos(x: Coordinate, y: Coordinate) -> Pos {
    Pos { x, y }
}

// -- problems

fn dijkstra(map: &TiledMap, from: Pos, to: Pos) -> Risk {
    let mut risks: Map<Risk> = Map::new(
        map.effective_width(), map.effective_height(), Risk::max_value()
    );
    let mut unvisited: HashSet<Pos> = risks.iter().collect();
    println!("{:?} unvisited locations", unvisited.len());
    risks[&from] = 0;

    let mut current = from;
    while current != to {
        unvisited.remove(&current);
        let current_risk = risks[&current];

        let neighbours: Vec<Pos> = current.neighbours()
            .filter(|n| unvisited.contains(n))
            .collect();

        neighbours.iter().for_each(|n| {
            let total_risk = current_risk + map.at(n);
            risks[n] = min(total_risk, risks[n]);
        });
        
        current = *unvisited.iter()
            .map(|p| (p, risks[p]))
            .min_by(|a, b| a.1.cmp(&b.1))
            .map(|(p, _)| p)
            .unwrap();
    }

    risks[&to]
}

fn part1(input: &str) -> Risk {
    let map = TiledMap::parse(&input, 1);
    let start = pos(0, 0);
    let end = pos(map.effective_width()-1, map.effective_height()-1);
    dijkstra(&map, start, end)
}

fn part2(input: &str) -> Risk {
    let map = TiledMap::parse(&input, 5);
    let start = pos(0, 0);
    let end = pos(map.effective_width()-1, map.effective_height()-1);
    dijkstra(&map, start, end)
}

fn main() {
    let input = std::fs::read_to_string("./input.txt")
        .expect("can't load input.txt");

    println!("Part 1: {:?}", part1(&input));
    println!("Part 2: {:?}", part2(&input));
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
    fn test_access_tiled_map() {
        let map = TiledMap::parse(example_input(), 5);
        assert_eq!(map.effective_width(), 50);
        assert_eq!(map.effective_height(), 50);
        assert_eq!(map.at(&pos(0, 0)), 1);
        assert_eq!(map.at(&pos(9, 0)), 2);
        assert_eq!(map.at(&pos(0, 9)), 2);
        assert_eq!(map.at(&pos(9, 9)), 1);
        assert_eq!(map.at(&pos(9, 2)), 8);

        assert_eq!(map.at(&pos(10, 10)), 3);
        assert_eq!(map.at(&pos(19, 10)), 4);
        assert_eq!(map.at(&pos(10, 19)), 4);
        assert_eq!(map.at(&pos(19, 19)), 3);

        assert_eq!(map.at(&pos(19, 2)), 9);
        assert_eq!(map.at(&pos(29, 2)), 1);
        assert_eq!(map.at(&pos(19, 12)), 1);
    }

    #[test]
    fn test_bounds_checks() {
        let map = TiledMap::parse(example_input(), 1);
        assert_eq!(map.contains(&pos(0, 0)), true);
        assert_eq!(map.contains(&pos(9, 9)), true);
        assert_eq!(map.contains(&pos(-1, 0)), false);
        assert_eq!(map.contains(&pos(0, -1)), false);
        assert_eq!(map.contains(&pos(10, 0)), false);
        assert_eq!(map.contains(&pos(0, 10)), false);
    }

    #[test]
    fn test_bounds_checks_with_multiplier() {
        let map = TiledMap::parse(example_input(), 5);
        assert_eq!(map.contains(&pos(0, 0)), true);
        assert_eq!(map.contains(&pos(49, 49)), true);
        assert_eq!(map.contains(&pos(-1, 0)), false);
        assert_eq!(map.contains(&pos(0, -1)), false);
        assert_eq!(map.contains(&pos(50, 0)), false);
        assert_eq!(map.contains(&pos(0, 50)), false);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(example_input()), 40);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(example_input()), 315);
    }

}