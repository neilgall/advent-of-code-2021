
use std::cmp::max;
use std::collections::HashMap;
use std::fmt;

// model

type Coord = usize;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct Point {
    pub x: Coord,
    pub y: Coord
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Direction {
    East,
    South
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Region {
    cucumbers: HashMap<Point, Direction>,
    width: Coord,
    height: Coord
}

impl Point {
    fn new(x: Coord, y: Coord) -> Self {
        Point { x, y }
    }
}

impl Region {
    fn from_string(input: &str) -> Self {
        let mut cucumbers = HashMap::new();
        let mut width: Coord = 0;
        let mut height: Coord = 0;
        for (y, line) in input.lines().filter(|line| !line.is_empty()).enumerate() {
            for (x, c) in line.trim().chars().enumerate() {
                width = max(width, x+1);
                height = max(height, y+1);
                match c {
                    '>' => {
                        cucumbers.insert(Point::new(x, y), Direction::East);
                    }
                    'v' => {
                        cucumbers.insert(Point::new(x, y), Direction::South);
                    }
                    _ => {}
                }
            }
        }

        Region { cucumbers, width, height }
    }

    fn at(&self, p: &Point) -> Option<&Direction> {
        self.cucumbers.get(p)
    }

    fn east(&self, p: &Point) -> Point {
        Point {
            x: (p.x + 1) % self.width,
            y: p.y
        }
    }

    fn south(&self, p: &Point) -> Point {
        Point {
            x: p.x,
            y: (p.y + 1) % self.height
        }
    }

    fn advance(&self, p: &Point, d: &Direction) -> Point {
        match d {
            Direction::East => self.east(p),
            Direction::South => self.south(p)
        }
    }

    fn step_direction(&self, direction: &Direction) -> Region {
        let mut cucumbers = HashMap::new();
        for (pos, kind) in self.cucumbers.iter() {
            if kind == direction && self.at(&self.advance(pos, direction)).is_none() {
                cucumbers.insert(self.advance(pos, direction), *direction);
            } else {
                cucumbers.insert(*pos, *kind);
            }
        }

        Region {
            cucumbers,
            width: self.width,
            height: self.height
        }
    }

    fn step(&self) -> Region {
        self.step_direction(&Direction::East)
            .step_direction(&Direction::South)
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                write!(f, "{}", match self.at(&Point::new(x, y)) {
                    None => '.',
                    Some(&Direction::East) => '>',
                    Some(&Direction::South) => 'v'
                })?;
            }
            write!(f, "\n")?;
        }
        write!(f, "")
    }
}

// problems

fn part1(region: &Region) -> usize {
    let mut steps: usize = 0;
    let mut prev: Region = region.clone();
    loop {
        steps += 1;
        let next = prev.step();
        if next == prev {
            return steps;
        }
        prev = next;
    }
}


fn main() {
    let input = std::fs::read_to_string("input.txt")
        .expect("Unable to read input.txt");
    let region = Region::from_string(&input);

    println!("Part 1: {}", part1(&region));
}


#[cfg(test)]
mod tests {
    use super::*;

    fn test_region() -> Region {
        Region::from_string(
            "v...>>.vv>
            .vv>>.vv..
            >>.>v>...v
            >>v>>.>.v.
            v>v.vv.v..
            >.>>..v...
            .vv..>.>v.
            v.v..>>v.v
            ....v..v.>"
        )
    }

    #[test]
    fn test_parse_region() {
        let region = test_region();
        assert_eq!(region.at(&Point::new(0, 0)), Some(&Direction::South));
        assert_eq!(region.at(&Point::new(5, 0)), Some(&Direction::East));
        assert_eq!(region.at(&Point::new(4, 2)), Some(&Direction::South));
        assert_eq!(region.at(&Point::new(8, 5)), None);
    }

    #[test]
    fn test_step() {
        let region = test_region();
        assert_eq!(region.step(), Region::from_string("
            ....>.>v.>
            v.v>.>v.v.
            >v>>..>v..
            >>v>v>.>.v
            .>v.v...v.
            v>>.>vvv..
            ..v...>>..
            vv...>>vv.
            >.v.v..v.v
        "));
    }

    #[test]
    fn test_part1() {
        let region = test_region();
        assert_eq!(part1(&region), 58);
    }

}