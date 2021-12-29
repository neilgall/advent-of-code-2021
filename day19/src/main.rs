#[macro_use]
extern crate lazy_static;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::fs;
use std::ops;

// model
type Coord = i32;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Axis {
    X, Y, Z
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Rotation {
    R0, R90, R180, R270
}

impl Rotation {
    fn iter() -> impl Iterator<Item = Self> + 'static {
        use Rotation::*;
        static VALUES: [Rotation; 4] = [R0, R90, R180, R270];
        VALUES.iter().copied()
    }

    fn reverse(&self) -> Self {
        use Rotation::*;
        match self {
            R0   => R0,
            R90  => R270,
            R180 => R180,
            R270 => R90
        }
    }
}

#[derive(Copy,Clone,Debug,Eq,Hash,PartialEq)]
struct Vector {
    pub x: Coord,
    pub y: Coord,
    pub z: Coord
}

impl Vector {
    fn new(x: Coord, y: Coord, z: Coord) -> Self {
        Vector { x, y, z }
    }

    fn rotate(&self, rot: Rotation, axis: Axis) -> Vector {
        use Rotation::*;
        use Axis::*;
        match (rot, axis) {
            (R0,   X) => *self,
            (R90,  X) => Vector::new( self.x,  self.z, -self.y),
            (R180, X) => Vector::new( self.x, -self.y, -self.z),
            (R270, X) => Vector::new( self.x, -self.z,  self.y),
            (R0,   Y) => *self,
            (R90,  Y) => Vector::new(-self.z,  self.y,  self.x),
            (R180, Y) => Vector::new(-self.x,  self.y, -self.z),
            (R270, Y) => Vector::new( self.z,  self.y, -self.x),
            (R0,   Z) => *self,
            (R90,  Z) => Vector::new( self.y, -self.x,  self.z),
            (R180, Z) => Vector::new(-self.x, -self.y,  self.z),
            (R270, Z) => Vector::new(-self.y,  self.x,  self.z)
        }
    }
}

impl Default for Vector {
    fn default() -> Self {
        Vector::new(0, 0, 0)
    }
}

impl ops::Add for Vector {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Vector {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z
        }
    }
}

impl ops::Sub for Vector {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Vector {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z
        }
    }
}


#[derive(Clone, Copy, Debug)]
struct Orientation {
    rotate_x: Rotation,
    rotate_y: Rotation,
    rotate_z: Rotation
}

lazy_static! {
    static ref ORIENTATIONS: Vec<Orientation> = {
        use Rotation::*;
        let directions = vec![
            (R0,   R0  ),  // pos X = no rotation
            (R90,  R0  ),  // pos Z, 90 around Y
            (R180, R0  ),  // neg X, 180 around Y
            (R270, R0  ),  // neg Z, 270 around Y
            (R0,   R90 ),  // neg Y, 90 around Z
            (R0,   R270)   // pos Y, 270 around Z
        ];
        
        let mut all: Vec<Orientation> = vec![];
        for (rotate_y, rotate_z) in directions.into_iter() {
            for rotate_x in Rotation::iter() {
                all.push(Orientation { rotate_x, rotate_y, rotate_z })
            }
        }
        all
    };
}

impl Orientation {
    fn iter() -> impl Iterator<Item = Orientation> + 'static {
        ORIENTATIONS.iter().copied()
    }

    fn new(rotate_x: Rotation, rotate_y: Rotation, rotate_z: Rotation) -> Self {
        Orientation { rotate_x, rotate_y, rotate_z }
    }

    fn normalise(&self, v: Vector) -> Vector {
        v.rotate(self.rotate_x, Axis::X)
         .rotate(self.rotate_y, Axis::Y)
         .rotate(self.rotate_z, Axis::Z)
    }
}

impl Default for Orientation {
    fn default() -> Self {
        Orientation {
            rotate_x: Rotation::R0,
            rotate_y: Rotation::R0,
            rotate_z: Rotation::R0
        }
    }
}

fn frequencies<T, I>(iter: I) -> HashMap<T, usize>
    where T: Copy + Eq + Hash,
          I: Iterator<Item = T> + Sized
{
    iter.fold(
        HashMap::new(),
        |mut map, val| {
            map.entry(val)
                .and_modify(|n| *n += 1)
                .or_insert(1);
            map
        }
    )
}


#[derive(Debug)]
struct Scanner {
    probes: Vec<Vector>,
    orientation: Orientation,
    position: Vector
}

impl Scanner {
    fn new(probes: Vec<Vector>) -> Self {
        Scanner {
            probes,
            orientation: Orientation::default(),
            position: Vector::default()
        }
    }

    fn position(&self) -> Vector {
        self.orientation.normalise(self.position)
    }

    fn oriented(&self, orientation: Orientation, position: Vector) -> Self {
        Scanner {
            probes: self.probes.clone(),
            orientation,
            position
        }
    }

    fn iter(&self) -> impl Iterator<Item = Vector> + '_ {
        self.probes.iter().map(move |p| self.orientation.normalise(*p))
    }

    fn candidate_vectors_to<'a>(&'a self, other: &'a Self, orientation: &'a Orientation)
    -> impl Iterator<Item = Vector> + 'a {
        self.iter().flat_map(move |p1|
            other.iter().map(move |p2| orientation.normalise(p1) - p2)
        )
    }

    fn find_vector_to(&self, other: &Self, orientation: &Orientation) -> Option<Vector> {
        let candidates = self.candidate_vectors_to(other, orientation);
        frequencies(candidates)
            .into_iter()
            .filter(|(_,count)| *count >= 12)
            .max_by_key(|(_, count)| *count)
            .map(|(vec, _)| vec)
    }

    fn locate_relative_to(&self, other: &Self) -> Option<Self> {
        Orientation::iter().find_map(|orientation|
            self.find_vector_to(other, &orientation).map(|vector|
                self.oriented(orientation, other.position() + vector)
            )
        )
    }
}

impl Clone for Scanner {
    fn clone(&self) -> Self {
        Scanner {
            probes: self.probes.clone(),
            orientation: self.orientation,
            position: self.position
        }
    }
}

// parser

fn parse_coord(s: &str) -> Coord {
    s.parse()
        .expect(&format!("cannot parse {}", s))
}

fn parse(input: &str) -> Vec<Scanner> {
    let mut scanners: Vec<Scanner> = vec![];
    let mut probes: Vec<Vector> = vec![];

    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }
        else if line.starts_with("--- scanner ") {
            if !probes.is_empty() {
                scanners.push(Scanner::new(probes));
               probes = vec![];
            }
        }
        else {
            let coords: Vec<Coord> = line.split(",").map(parse_coord).collect();
            assert!(coords.len() == 3);
            probes.push(Vector::new(coords[0], coords[1], coords[2]));
        }
    }

    if !probes.is_empty() {
        scanners.push(Scanner::new(probes));
    }

    scanners
}


// problems

fn match_scanners(candidates: &Vec<Scanner>, match_to: &Vec<Scanner>) -> (usize, Scanner) {
    candidates.iter().enumerate().find_map(|(i,c)|
        match_to.iter()
            .find_map(|m| c.locate_relative_to(m))
            .map(|r| (i,r))
    )
    .expect("can't match any scanners")
}

fn orient_scanners(scanners: &Vec<Scanner>) -> Vec<Scanner> {
    let mut oriented: Vec<Scanner> = vec![];
    let mut remaining: Vec<Scanner> = scanners.clone();
    oriented.push(scanners[0].clone());
    remaining.remove(0);
    while !remaining.is_empty() {
        println!("oriented {} remaining {}", oriented.len(), remaining.len());
        let (i, s) = match_scanners(&remaining, &oriented);
        oriented.push(s);
        remaining.remove(i);
    }
    oriented
}

fn count_all_beacons(scanners: &Vec<Scanner>) -> usize {
    let mut beacons: HashSet<Vector> = HashSet::new();
    for scanner in orient_scanners(&scanners) {
        scanner.iter().for_each(|b| { beacons.insert(b); });
    }
    beacons.len()
}


fn main() {
    let input = std::fs::read_to_string("input.txt")
        .expect("cannot read input file");
    let scanners = parse(&input);

    println!("Part 1: {}", count_all_beacons(&scanners));
}


#[cfg(test)]
mod tests {
    use super::*;

    fn test_input() -> String {
        std::fs::read_to_string("example.txt")
            .expect("cannot read example file")
    }
    
    #[test]
    fn test_parser() {
        let scanners = parse(&test_input());
        assert_eq!(scanners.len(), 5);
        assert_eq!(scanners[0].probes.len(), 25);
        assert_eq!(scanners[4].probes.len(), 26);
        assert_eq!(scanners[0].probes[8], Vector::new(-876,649,763));
        assert_eq!(scanners[3].probes[3], Vector::new(-660,373,557));
    }

    #[test]
    fn test_rotate() {
        use Rotation::*;
        use Axis::*;
        let x = Vector::new(1, 0, 0);
        let y = Vector::new(0, 1, 0);
        let z = Vector::new(0, 0, 1);

        assert_eq!(x.rotate(R0,   X), x);
        assert_eq!(x.rotate(R90,  X), x);
        assert_eq!(x.rotate(R180, X), x);
        assert_eq!(x.rotate(R270, X), x);

        assert_eq!(x.rotate(R0,   Y), x);
        assert_eq!(x.rotate(R90,  Y), Vector::new( 0,  0,  1));
        assert_eq!(x.rotate(R180, Y), Vector::new(-1,  0,  0));
        assert_eq!(x.rotate(R270, Y), Vector::new( 0,  0, -1));

        assert_eq!(x.rotate(R0,   Z), x);
        assert_eq!(x.rotate(R90,  Z), Vector::new( 0, -1,  0));
        assert_eq!(x.rotate(R180, Z), Vector::new(-1,  0,  0));
        assert_eq!(x.rotate(R270, Z), Vector::new( 0,  1,  0));

        assert_eq!(y.rotate(R0,   X), y);
        assert_eq!(y.rotate(R90,  X), Vector::new( 0,  0, -1));
        assert_eq!(y.rotate(R180, X), Vector::new( 0, -1,  0));
        assert_eq!(y.rotate(R270, X), Vector::new( 0,  0,  1));

        assert_eq!(y.rotate(R0,   Y), y);
        assert_eq!(y.rotate(R90,  Y), y);
        assert_eq!(y.rotate(R180, Y), y);
        assert_eq!(y.rotate(R270, Y), y);

        assert_eq!(y.rotate(R0,   Z), y);
        assert_eq!(y.rotate(R90,  Z), Vector::new( 1,  0,  0));
        assert_eq!(y.rotate(R180, Z), Vector::new( 0, -1,  0));
        assert_eq!(y.rotate(R270, Z), Vector::new(-1,  0,  0));

        assert_eq!(z.rotate(R0,   X), z);
        assert_eq!(z.rotate(R90,  X), Vector::new( 0,  1,  0));
        assert_eq!(z.rotate(R180, X), Vector::new( 0,  0, -1));
        assert_eq!(z.rotate(R270, X), Vector::new( 0, -1,  0));

        assert_eq!(z.rotate(R0,   Y), z);
        assert_eq!(z.rotate(R90,  Y), Vector::new(-1,  0,  0));
        assert_eq!(z.rotate(R180, Y), Vector::new( 0,  0, -1));
        assert_eq!(z.rotate(R270, Y), Vector::new( 1,  0,  0));

        assert_eq!(z.rotate(R0,   Z), z);
        assert_eq!(z.rotate(R90,  Z), z);
        assert_eq!(z.rotate(R180, Z), z);
        assert_eq!(z.rotate(R270, Z), z);
    }

    #[test]
    fn test_unique_orientations() {
        assert_eq!(Orientation::iter().count(), 24);
    }

    #[test]
    fn test_transformations() {
        fn check(index: usize, expect: Vec<(Coord,Coord,Coord)>) -> bool {
            let scanner = Scanner::new(vec![
                Vector::new(-1,-1,-1),
                Vector::new(-2,-2,2),
                Vector::new(-3,-3,3),
                Vector::new(-2,-3,1),
                Vector::new(5,6,-4),
                Vector::new(8,0,7)
            ]);
            let probe: Vec<Vector> = Orientation::iter().map(|orientation|
                scanner.oriented(orientation, Vector::new(0,0,0))
                    .iter()
                    .nth(index)
                    .unwrap()
            ).collect();
            expect.iter().all(|(x,y,z)| {
                let r = probe.contains(&Vector::new(*x, *y, *z));
                if !r {
                    print!("probe ");
                    probe.iter().for_each(|b| print!("({},{},{}) ", b.x, b.y, b.z));
                    println!("does not contain ({},{},{})", x, y, z);
                }
                r
            })
        }
        assert!(check(0, vec![(-1,-1,1), (1,-1,1), (-1,-1,-1), (1,1,-1), (1,1,1)]));
        assert!(check(1, vec![(-2,-2,2), (2,-2,2), (-2,-2,-2), (2,2,-2), (2,2,2)]));
        assert!(check(2, vec![(-3,-3,3), (3,-3,3), (-3,-3,-3), (3,3,-3), (3,3,3)]));
        assert!(check(3, vec![(-2,-3,1), (2,-1,3), (-1,-3,-2), (1,3,-2), (3,1,2)]));
        assert!(check(4, vec![(5,6,-4), (-5,4,-6), (4,6,5), (-4,-6,5), (-6,-4,-5)]));
        assert!(check(5, vec![(8,0,7), (-8,-7,0), (-7,0,8), (7,0,8), (0,7,-8)]));
    }

    #[test]
    fn test_find_vector_to() {
        let scanners = parse(&test_input());
        let scanner_0_to_1 = Vector::new(68,-1246,-43);
        let scanner_0_to_4 = Vector::new(-20,-1133,1061);

        assert_eq!(
            scanners[0].locate_relative_to(&scanners[1]).map(|s| s.position()),
            Some(scanner_0_to_1)
        );
        assert_eq!(
            scanners[1].locate_relative_to(&scanners[4]).map(|s| s.position()),
            Some(scanner_0_to_4 - scanner_0_to_1)
        )
    }

    #[test]
    fn test_orient_scanners() {
        let scanners = parse(&test_input());
        let oriented = orient_scanners(&scanners);
        assert_eq!(oriented[0].position(), Vector::new(0, 0, 0));
        assert_eq!(oriented[1].position(), Vector::new(68, -1246, -43));
        assert_eq!(oriented[2].position(), Vector::new(1105, -1205, 1229));
        assert_eq!(oriented[3].position(), Vector::new(-92, -2380, -20));
        assert_eq!(oriented[4].position(), Vector::new(-20, -1133, 1061));
    }

    #[test]
    fn test_count_beacons() {
        let scanners = parse(&test_input());
        assert_eq!(count_all_beacons(&scanners), 79);
    }
}