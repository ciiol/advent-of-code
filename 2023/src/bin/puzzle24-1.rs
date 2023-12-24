// https://adventofcode.com/2023/day/24
// Due to strong, probably-magical winds, the hailstones are all flying through the air in perfectly
// linear trajectories. You make a note of each hailstone's position and velocity (your puzzle input).
//
// Look for intersections that happen with an X and Y position each at least 200000000000000 and
// at most 400000000000000. Disregard the Z axis entirely.
//
// Considering only the X and Y axes, check all pairs of hailstones' future paths for intersections.
// How many of these intersections occur within the test area?
extern crate advent_of_code_2023 as advent;

use std::io::Error;
use std::ops::Range;

use nalgebra::Vector2;
use nom::bytes::complete::tag;
use nom::character::complete::{newline, space0};
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::number::complete::double;
use nom::sequence::{separated_pair, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

type Position = Vector2<f64>;
type Velocity = Vector2<f64>;

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

fn vector(input: &str) -> IResult<&str, Vector2<f64>> {
    map(separated_list1(tuple((tag(","), space0)), double), |v| {
        Vector2::new(v[0], v[1])
    })(input)
}

fn puzzle(input: &[Hailstone]) -> usize {
    find_collisions_within(input, 200_000_000_000_000.0..400_000_000_000_000.0).len()
}

fn find_collision(first: &Hailstone, second: &Hailstone) -> Position {
    // Let's write paths equation as y = a * x + b
    let a1 = first.velocity.y / first.velocity.x;
    let b1 = first.position.y - a1 * first.position.x;
    let a2 = second.velocity.y / second.velocity.x;
    let b2 = second.position.y - a2 * second.position.x;
    let x = (b2 - b1) / (a1 - a2);
    let y = a1 * x + b1;
    Vector2::new(x, y)
}

fn is_future(hailstone: &Hailstone, position: &Position) -> bool {
    let diff = position - hailstone.position;
    diff.x / hailstone.velocity.x > 0.0 && diff.y / hailstone.velocity.y > 0.0
}

fn find_collisions_within(hailstones: &[Hailstone], range: Range<f64>) -> Vec<Position> {
    let mut collisions = Vec::new();
    for (i, first) in hailstones.iter().enumerate() {
        for second in hailstones.iter().skip(i + 1) {
            let collision = find_collision(first, second);
            if is_future(first, &collision)
                && is_future(second, &collision)
                && range.contains(&collision.x)
                && range.contains(&collision.y)
            {
                collisions.push(collision);
            }
        }
    }
    collisions
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "19, 13, 30 @ -2,  1, -2\n\
                     18, 19, 22 @ -1, -1, -2\n\
                     20, 25, 34 @ -2, -2, -4\n\
                     12, 31, 28 @ -1, -2, -1\n\
                     20, 19, 15 @  1, -5, -3";
        let hailstones = parse(input, puzzle_input)?;
        let collisions = find_collisions_within(&hailstones, 7.0..20.0);
        assert_eq!(collisions.len(), 2);
        Ok(())
    }
}
