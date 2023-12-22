// https://adventofcode.com/2023/day/22#part2
// The Elves responsible for water filtering operations took a snapshot of the bricks while they were still
// falling (your puzzle input). Each line of text in the snapshot represents the position of a single brick at
// the time the snapshot was taken. The position is given as two x,y,z coordinates - one for each end of
// the brick - separated by a tilde (~). Bricks are magically stabilized, so they never rotate.
//
// Figure how the blocks will settle based on the snapshot.
//
// For each brick, determine how many other bricks would fall if that brick were disintegrated.
// What is the sum of the number of other bricks that would fall?
extern crate advent_of_code_2023 as advent;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::io::Error;

use nalgebra::Vector3;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as i64_parser, newline};
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::separated_pair;
use nom::{IResult, Parser};

use advent::runner::run_puzzle;

type Point = Vector3<i64>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct Brick {
    a: Point,
    b: Point,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Space {
    bricks: Vec<Brick>,
    index: HashMap<Point, usize>,
}

impl Brick {
    fn new(a: Point, b: Point) -> Self {
        assert!(a.z <= b.z);
        Self { a, b }
    }

    fn points(&self) -> impl Iterator<Item = Point> + '_ {
        let x_range = self.a.x..=self.b.x;
        x_range.flat_map(move |x| {
            let y_range = self.a.y..=self.b.y;
            y_range.flat_map(move |y| {
                let z_range = self.a.z..=self.b.z;
                z_range.clone().map(move |z| Point::new(x, y, z))
            })
        })
    }

    fn is_horizontal(&self) -> bool {
        self.a.z == self.b.z
    }

    fn points_above(&self) -> Vec<Point> {
        let change = Vector3::new(0, 0, 1);
        if self.is_horizontal() {
            self.points().map(|p| p + change).collect()
        } else {
            vec![self.b + change]
        }
    }

    fn points_below(&self) -> Vec<Point> {
        let change = Vector3::new(0, 0, -1);
        if self.is_horizontal() {
            self.points().map(|p| p + change).collect()
        } else {
            vec![self.a + change]
        }
    }
}

impl Space {
    fn new(bricks: Vec<Brick>) -> Self {
        let mut index = HashMap::new();
        for (i, brick) in bricks.iter().enumerate() {
            for point in brick.points() {
                index.insert(point, i);
            }
        }
        Self { bricks, index }
    }

    fn is_supported(&self, brick: &Brick) -> bool {
        if brick.a.z == 1 || brick.b.z == 1 {
            return true;
        }
        brick
            .points_below()
            .into_iter()
            .any(|p| self.index.contains_key(&p))
    }

    fn supported_by(&self, brick: &Brick) -> HashSet<&Brick> {
        brick
            .points_below()
            .into_iter()
            .filter_map(|p| self.index.get(&p))
            .map(|&i| &self.bricks[i])
            .collect()
    }

    fn supports(&self, brick: &Brick) -> HashSet<&Brick> {
        brick
            .points_above()
            .into_iter()
            .filter_map(|p| self.index.get(&p))
            .map(|&i| &self.bricks[i])
            .collect()
    }

    fn settle_bricks(&mut self) {
        let change = Point::new(0, 0, -1);
        loop {
            let without_support: Vec<_> = self
                .bricks
                .iter()
                .enumerate()
                .filter_map(|(i, b)| (!self.is_supported(b)).then_some(i))
                .collect();
            if without_support.is_empty() {
                break;
            }
            for i in without_support {
                let brick = self.bricks[i];
                for p in brick.points() {
                    self.index.remove(&p);
                }
                let brick = Brick::new(brick.a + change, brick.b + change);
                self.index.extend(brick.points().map(|p| (p, i)));
                self.bricks[i] = brick;
            }
        }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle22.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Brick>> {
    let brick = map(separated_pair(point, tag("~"), point), |(a, b)| Brick {
        a,
        b,
    });
    separated_list1(newline, brick).parse(input)
}

fn point(input: &str) -> IResult<&str, Point> {
    let mut point = map(separated_list1(tag(","), i64_parser), |v| {
        Point::new(v[0], v[1], v[2])
    });
    point.parse(input)
}

fn puzzle(input: &[Brick]) -> usize {
    let mut space = Space::new(input.to_vec());
    space.settle_bricks();
    space.bricks.iter().map(|b| chain_fall(&space, b)).sum()
}

fn chain_fall(space: &Space, brick: &Brick) -> usize {
    let mut to_fall = vec![brick];
    let mut removed = HashSet::from([brick]);
    while let Some(b) = to_fall.pop() {
        let supports: Vec<_> = space
            .supports(b)
            .into_iter()
            .filter(|s| space.supported_by(s).is_subset(&removed))
            .collect();
        removed.extend(supports.iter().copied());
        to_fall.extend(supports);
    }
    removed.len() - 1
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "1,0,1~1,2,1\n\
                     0,0,2~2,0,2\n\
                     0,2,3~2,2,3\n\
                     0,0,4~0,2,4\n\
                     2,0,5~2,2,5\n\
                     0,1,6~2,1,6\n\
                     1,1,8~1,1,9";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 7);
        Ok(())
    }
}
