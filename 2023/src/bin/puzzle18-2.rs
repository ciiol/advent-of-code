// https://adventofcode.com/2023/day/18
// They've asked you to take a look at the dig plan (your puzzle input). The digger starts in a 1 meter cube hole
// in the ground. They then dig the specified number of meters up (U), down (D), left (L), or right (R), clearing
// full 1 meter cubes as they go. The directions are given as seen from above, so if "up" were north, then "right"
// would be east, and so on. Each trench is also listed with the color that the edge of the trench should be
// painted as an RGB hexadecimal color code.
//
// The next step is to dig out the interior so that it is one meter deep as well.
//
// The Elves are concerned the lagoon won't be large enough. After a few minutes, someone realizes what happened;
// someone swapped the color and instruction parameters when producing the dig plan. They don't have time to fix
// the bug; one of them asks if you can extract the correct instructions from the hexadecimal codes.
//
// Each hexadecimal code is six hexadecimal digits long. The first five hexadecimal digits encode the distance
// in meters as a five-digit hexadecimal number. The last hexadecimal digit encodes the direction to dig:
// 0 means R, 1 means D, 2 means L, and 3 means U.
//
// Convert the hexadecimal color codes into the correct instructions; if the Elves follow this new dig plan,
// how many cubic meters of lava could the lagoon hold?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nalgebra::Vector2;
use nom::bytes::complete::tag;
use nom::character::complete::{hex_digit1, newline, none_of};
use nom::combinator::map;
use nom::multi::{many1, separated_list1};
use nom::sequence::{delimited, preceded};
use nom::IResult;

use advent::runner::run_puzzle;

type Vector = Vector2<i64>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Action {
    pub direction: Vector,
    pub amount: u32,
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle18.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Action>> {
    let action = map(
        preceded(
            many1(none_of::<&str, &str, _>("(")),
            delimited(tag("("), preceded(tag("#"), hex_digit1), tag(")")),
        ),
        |color| {
            let direction = match color.chars().last().unwrap() {
                '0' => Vector::new(1, 0),
                '1' => Vector::new(0, -1),
                '2' => Vector::new(-1, 0),
                '3' => Vector::new(0, 1),
                _ => panic!("Invalid direction"),
            };
            let amount = u32::from_str_radix(&color[..color.len() - 1], 16).unwrap();
            Action { direction, amount }
        },
    );
    separated_list1(newline, action)(input)
}

fn puzzle(input: &[Action]) -> usize {
    let mut current = Vector::new(0, 0);
    let mut figure = vec![];
    for action in input {
        current = apply_action(current, action);
        figure.push(current);
    }
    gauss_area(&figure)
}

fn apply_action(position: Vector, action: &Action) -> Vector {
    let amount = i64::from(action.amount);
    let direction = action.direction;
    position + direction * amount
}

fn gauss_area(figure: &[Vector]) -> usize {
    let mut area = 0;
    let mut boundary = 0;
    for (p1, p2) in figure.iter().zip(figure.iter().cycle().skip(1)) {
        boundary += (p2 - p1).map(i64::abs).sum();
        area += p1.x * p2.y - p2.x * p1.y;
    }
    usize::try_from((area.abs() + boundary) / 2 + 1).unwrap()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "R 6 (#70c710)\n\
                     D 5 (#0dc571)\n\
                     L 2 (#5713f0)\n\
                     D 2 (#d2c081)\n\
                     R 2 (#59c680)\n\
                     D 2 (#411b91)\n\
                     L 5 (#8ceee2)\n\
                     U 2 (#caa173)\n\
                     L 1 (#1b58a2)\n\
                     U 2 (#caa171)\n\
                     R 2 (#7807d2)\n\
                     U 3 (#a77fa3)\n\
                     L 2 (#015232)\n\
                     U 2 (#7a21e3)";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 952_408_144_115);
        Ok(())
    }
}
