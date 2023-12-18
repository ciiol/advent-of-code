// https://adventofcode.com/2023/day/18
// They've asked you to take a look at the dig plan (your puzzle input). The digger starts in a 1 meter cube hole
// in the ground. They then dig the specified number of meters up (U), down (D), left (L), or right (R), clearing
// full 1 meter cubes as they go. The directions are given as seen from above, so if "up" were north, then "right"
// would be east, and so on. Each trench is also listed with the color that the edge of the trench should be
// painted as an RGB hexadecimal color code.
//
// The next step is to dig out the interior so that it is one meter deep as well.
//
// The Elves are concerned the lagoon won't be large enough;
// if they follow their dig plan, how many cubic meters of lava could it hold?
extern crate advent_of_code_2023 as advent;

use std::collections::HashSet;
use std::io::Error;

use nalgebra::Vector2;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::u32 as u32_parser;
use nom::character::complete::{hex_digit1, newline, space1};
use nom::combinator::{map, value};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, tuple};
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
    let direction = alt((
        value(Vector::new(0, 1), tag("U")),
        value(Vector::new(0, -1), tag("D")),
        value(Vector::new(-1, 0), tag("L")),
        value(Vector::new(1, 0), tag("R")),
    ));
    let amount = u32_parser;
    let color = delimited(tag("("), preceded(tag("#"), hex_digit1), tag(")"));
    let action = map(
        tuple((direction, space1, amount, space1, color)),
        |(direction, _, amount, _, _)| Action { direction, amount },
    );
    separated_list1(newline, action)(input)
}

fn puzzle(input: &[Action]) -> usize {
    let mut figure = vec![Vector::new(0, 0)];
    for action in input {
        let last = *figure.last().unwrap();
        figure.extend(apply_action(last, action));
    }
    figure_area(&figure)
}

fn apply_action(position: Vector, action: &Action) -> impl Iterator<Item = Vector> + '_ {
    let amount = i64::from(action.amount);
    let direction = action.direction;
    (1..=amount).map(move |i| position + direction * i)
}

fn figure_area(figure: &[Vector]) -> usize {
    let min_x = figure.iter().map(|v| v.x).min().unwrap() - 1;
    let max_x = figure.iter().map(|v| v.x).max().unwrap() + 1;
    let min_y = figure.iter().map(|v| v.y).min().unwrap() - 1;
    let max_y = figure.iter().map(|v| v.y).max().unwrap() + 1;
    let figure: HashSet<_> = figure.iter().collect();
    let mut visited = HashSet::new();
    let mut stack = vec![Vector::new(min_x, min_y)];
    while let Some(point) = stack.pop() {
        if visited.contains(&point) || figure.contains(&point) {
            continue;
        }
        visited.insert(point);
        let neighbours = [
            Vector::new(0, 1),
            Vector::new(0, -1),
            Vector::new(1, 0),
            Vector::new(-1, 0),
        ];
        stack.extend(neighbours.into_iter().filter_map(|v| {
            let neighbour = point + v;
            (neighbour.x >= min_x
                && neighbour.x <= max_x
                && neighbour.y >= min_y
                && neighbour.y <= max_y)
                .then_some(neighbour)
        }));
    }
    let total_area =
        usize::try_from(max_x - min_x + 1).unwrap() * usize::try_from(max_y - min_y + 1).unwrap();
    total_area - visited.len()
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
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 62);
        Ok(())
    }
}
