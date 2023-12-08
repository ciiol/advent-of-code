// https://adventofcode.com/2023/day/8
// One of the camel's pouches is labeled "maps" - sure enough, it's full of documents
// (your puzzle input) about how to navigate the desert. It seems like you're meant
// to use the left/right instructions to navigate the network.
//
// After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like
// AAA is where you are now, and you have to follow the left/right instructions
// until you reach ZZZ.
//
// Starting at AAA, follow the left/right instructions.
// How many steps are required to reach ZZZ?
extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
use std::io::Error;
use std::iter::repeat;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, newline};
use nom::combinator::{map, value};
use nom::multi::{many1, separated_list1};
use nom::sequence::{delimited, separated_pair};
use nom::IResult;

use advent::runner::run_puzzle;

type Node = String;
type Fork = (Node, Node);
type Network = HashMap<Node, Fork>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Direction {
    L,
    R,
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle08.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, (Vec<Direction>, Network)> {
    let directions = many1(alt((
        value(Direction::L, tag("L")),
        value(Direction::R, tag("R")),
    )));
    let fork = delimited(tag("("), separated_pair(node, tag(", "), node), tag(")"));
    let network_node = separated_pair(node, tag(" = "), fork);
    let network = map(separated_list1(newline, network_node), |nodes| {
        nodes.into_iter().collect()
    });
    separated_pair(directions, tag("\n\n"), network)(input)
}

fn node(input: &str) -> IResult<&str, Node> {
    map(alphanumeric1, String::from)(input)
}

fn puzzle(input: &(Vec<Direction>, Network)) -> usize {
    let (directions, network) = input;
    let mut node = "AAA";
    for (i, direction) in repeat(directions).flatten().enumerate() {
        let (left, right) = network.get(node).unwrap();
        node = match direction {
            Direction::L => left,
            Direction::R => right,
        };
        if node == "ZZZ" {
            return i + 1;
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle1() -> Result<(), Error> {
        let input = "RL\n\
                     \n\
                     AAA = (BBB, CCC)\n\
                     BBB = (DDD, EEE)\n\
                     CCC = (ZZZ, GGG)\n\
                     DDD = (DDD, DDD)\n\
                     EEE = (EEE, EEE)\n\
                     GGG = (GGG, GGG)\n\
                     ZZZ = (ZZZ, ZZZ)";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 2);
        Ok(())
    }

    #[test]
    fn test_puzzle2() -> Result<(), Error> {
        let input = "LLR\n\
                     \n\
                     AAA = (BBB, BBB)\n\
                     BBB = (AAA, ZZZ)\n\
                     ZZZ = (ZZZ, ZZZ)";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 6);
        Ok(())
    }
}
