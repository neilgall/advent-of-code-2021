use std::collections::HashSet;
use std::ops::RangeInclusive;

// model

type Coord = i32;

#[derive(Debug, Eq, Hash, PartialEq)]
struct Cube {
    x: Coord,
    y: Coord,
    z: Coord
}

#[derive(Debug, Eq, PartialEq)]
struct Cuboid {
    x: RangeInclusive<Coord>,
    y: RangeInclusive<Coord>,
    z: RangeInclusive<Coord>
}

impl Cube {
    fn new(x: Coord, y: Coord, z: Coord) -> Self {
        Cube { x, y, z }
    }
}

impl Cuboid {
    fn from(s: &str) -> Self {
        fn parse_range(s: Option<&str>, p: &str) -> RangeInclusive<Coord> {
            let (start, end) = s.and_then(|r| r.split_once(".."))
                .expect("expected range");
            let start = start.trim_start_matches(p)
                .parse()
                .expect(&format!("expected coord; got {}", start));
            let end = end.parse()
                .expect(&format!("expected coord, got {}", end));

            start..=end
        }

        let mut parts = s.splitn(3, ",");
        let x = parse_range(parts.next(), "x=");
        let y = parse_range(parts.next(), "y=");
        let z = parse_range(parts.next(), "z=");
        
        Cuboid { x, y, z }
    }

    fn iter(&self) -> impl Iterator<Item = Cube> + '_ {
        self.z.clone().flat_map(move |z|
            self.y.clone().flat_map(move |y|
                self.x.clone().map(move |x| Cube { x, y, z })
            )
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Instruction {
    Off(Cuboid),
    On(Cuboid)
}

impl Instruction {
    fn from(s: &str) -> Instruction {
        let mut parts = s.split(" ");
        let instr = parts.next().expect("expected instruction");
        let cuboid = Cuboid::from(parts.next().expect("expected cuboid"));
        match instr {
            "off" => Instruction::Off(cuboid),
            "on" => Instruction::On(cuboid),
            _ => panic!("expected on or off")
        }
    }
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input.lines().map(Instruction::from).collect()
}


// problems

fn in_part1_range(cuboid: &Cuboid) -> bool {
    let valid = -50..=50;
    valid.contains(cuboid.x.start())
        && valid.contains(cuboid.x.end())
        && valid.contains(cuboid.y.start())
        && valid.contains(cuboid.y.end())
        && valid.contains(cuboid.z.start())
        && valid.contains(cuboid.z.end())
}

fn part1(instructions: &Vec<Instruction>) -> usize {
    let mut cubes: HashSet<Cube> = HashSet::new();
    instructions.iter()
        .for_each(|instr| match instr {
            Instruction::On(cuboid) if in_part1_range(cuboid) => {
                cuboid.iter().for_each(|c| { cubes.insert(c); })
            }
            Instruction::Off(cuboid) if in_part1_range(cuboid) => {
                cuboid.iter().for_each(|c| { cubes.remove(&c); })
            }
            _ => {}
        });
    cubes.len()
}


fn main() {
    let input = std::fs::read_to_string("input.txt")
        .expect("can't read input.txt");
    let instructions = parse_input(&input);

    println!("Part 1: {}", part1(&instructions));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_cube() {
        assert_eq!(
            Cuboid::from("x=10..12,y=11..13,z=9..11"),
            Cuboid { x: 10..=12, y: 11..=13, z: 9..= 11 }
        );
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(
            Instruction::from("on x=11..13,y=11..13,z=11..13"),
            Instruction::On(
                Cuboid { x: 11..=13, y: 11..=13, z: 11..=13 }
            )
        );

        assert_eq!(
            Instruction::from("off x=-48..-32,y=26..41,z=-47..-37"),
            Instruction::Off(
                Cuboid { x: -48..=-32, y: 26..=41, z: -47..=-37 }
            )
        );
    }

    #[test]
    fn test_cuboid_iter() {
        let cuboid = Cuboid::from("x=10..12,y=11..13,z=9..11");
        let cubes: Vec<Cube> = cuboid.iter().collect();
        assert!(cubes.contains(&Cube::new(10,11,9)));
        assert!(cubes.contains(&Cube::new(11,11,9)));
        assert!(cubes.contains(&Cube::new(12,11,9)));
        assert!(cubes.contains(&Cube::new(10,12,9)));
        assert!(cubes.contains(&Cube::new(11,12,9)));
        assert!(cubes.contains(&Cube::new(12,12,9)));
        assert!(cubes.contains(&Cube::new(10,13,9)));
        assert!(cubes.contains(&Cube::new(11,13,9)));
        assert!(cubes.contains(&Cube::new(12,13,9)));
        assert!(cubes.contains(&Cube::new(10,11,10)));
        assert!(cubes.contains(&Cube::new(11,11,10)));
        assert!(cubes.contains(&Cube::new(12,11,10)));
        assert!(cubes.contains(&Cube::new(10,12,10)));
        assert!(cubes.contains(&Cube::new(11,12,10)));
        assert!(cubes.contains(&Cube::new(12,12,10)));
        assert!(cubes.contains(&Cube::new(10,13,10)));
        assert!(cubes.contains(&Cube::new(11,13,10)));
        assert!(cubes.contains(&Cube::new(12,13,10)));
        assert!(cubes.contains(&Cube::new(10,11,11)));
        assert!(cubes.contains(&Cube::new(11,11,11)));
        assert!(cubes.contains(&Cube::new(12,11,11)));
        assert!(cubes.contains(&Cube::new(10,12,11)));
        assert!(cubes.contains(&Cube::new(11,12,11)));
        assert!(cubes.contains(&Cube::new(12,12,11)));
        assert!(cubes.contains(&Cube::new(10,13,11)));
        assert!(cubes.contains(&Cube::new(11,13,11)));
        assert!(cubes.contains(&Cube::new(12,13,11)));
    }

    fn test_data() -> Vec<Instruction> {
        parse_input("
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682        
".trim())
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(&test_data()), 590784);
    }
}