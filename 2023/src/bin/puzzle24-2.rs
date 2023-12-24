// https://adventofcode.com/2023/day/24#part2
// Due to strong, probably-magical winds, the hailstones are all flying through the air in perfectly
// linear trajectories. You make a note of each hailstone's position and velocity (your puzzle input).
//
// You find a rock on the ground nearby. While it seems extremely unlikely, if you throw it just right,
// you should be able to hit every hailstone in a single throw!
//
// You can use the probably-magical winds to reach any integer position you like and to propel the rock
// at any integer velocity. Now including the Z axis in your calculations, if you throw the rock at time 0,
// where do you need to be so that the rock perfectly collides with every hailstone? Due to probably-magical
// inertia, the rock won't slow down or change direction when it collides with a hailstone.
//
// Determine the exact position and velocity the rock needs to have at time 0 so that it perfectly collides
// with every hailstone. What do you get if you add up the X, Y, and Z coordinates of that initial position?
extern crate advent_of_code_2023 as advent;

use std::io::Error;
use std::ops::Mul;

use nalgebra::{RowVector3, Vector3};
use nom::bytes::complete::tag;
use nom::character::complete::{newline, space0};
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::number::complete::double;
use nom::sequence::{separated_pair, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

type Position = Vector3<f64>;
type Velocity = Vector3<f64>;

#[derive(Debug, PartialEq, Copy, Clone)]
struct Hailstone {
    position: Position,
    velocity: Velocity,
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle24.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Hailstone>> {
    let hailstone = map(
        separated_pair(vector, tuple((space0, tag("@"), space0)), vector),
        |(position, velocity)| Hailstone { position, velocity },
    );
    separated_list1(newline, hailstone)(input)
}

fn vector(input: &str) -> IResult<&str, Vector3<f64>> {
    map(separated_list1(tuple((tag(","), space0)), double), |v| {
        Vector3::new(v[0], v[1], v[2])
    })(input)
}

fn puzzle(input: &[Hailstone]) -> f64 {
    let (p1, t1) = find_crossing_plane_and_line(&input[1], &input[2], &input[0]);
    let (p2, t2) = find_crossing_plane_and_line(&input[1], &input[3], &input[0]);
    let velocity = (p2 - p1) / (t2 - t1);
    let position = p1 - velocity * t1;
    position.sum()
}

fn find_crossing_plane_and_line(a: &Hailstone, b: &Hailstone, c: &Hailstone) -> (Position, f64) {
    let p1 = a.position - c.position;
    let p2 = b.position - c.position;
    let v1 = a.velocity - c.velocity;
    let v2 = b.velocity - c.velocity;
    let matrix = nalgebra::Matrix3::from_rows(&[
        RowVector3::new(p1.x, v1.x, -v2.x),
        RowVector3::new(p1.y, v1.y, -v2.y),
        RowVector3::new(p1.z, v1.z, -v2.z),
    ]);
    let vector = Vector3::new(p2.x, p2.y, p2.z);
    let inverted = matrix.try_inverse().unwrap();
    let solution = inverted.mul(vector);
    let t = solution[2];
    let position = b.position + b.velocity * t;
    (position, t)
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    #[allow(clippy::cast_possible_truncation)]
    fn test_puzzle() -> Result<(), Error> {
        let input = "19, 13, 30 @ -2,  1, -2\n\
                     18, 19, 22 @ -1, -1, -2\n\
                     20, 25, 34 @ -2, -2, -4\n\
                     12, 31, 28 @ -1, -2, -1\n\
                     20, 19, 15 @  1, -5, -3";
        assert_eq!(puzzle(&parse(input, puzzle_input)?) as i64, 47);
        Ok(())
    }
}
