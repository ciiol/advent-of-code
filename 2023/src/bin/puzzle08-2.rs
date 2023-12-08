// https://adventofcode.com/2023/day/8
// One of the camel's pouches is labeled "maps" - sure enough, it's full of documents
// (your puzzle input) about how to navigate the desert. It seems like you're meant
// to use the left/right instructions to navigate the network.
//
// After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like
// AAA is where you are now, and you have to follow the left/right instructions
// until you reach ZZZ.
//
// After examining the maps a bit longer, your attention is drawn to a curious fact:
// the number of nodes with names ending in A is equal to the number ending in Z!
// If you were a ghost, you'd probably just start at every node that ends with A
// and follow all of the paths at the same time until they all simultaneously end
// up at nodes that end with Z.
//
// Simultaneously start on every node that ends with A. How many steps does it take
// before you're only on nodes that end with Z?
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
    let nodes: Vec<&Node> = network.keys().filter(|n| n.ends_with('A')).collect();
    let periods: HashMap<&Node, (usize, usize)> = nodes
        .iter()
        .map(|&n| (n, find_period(n, directions, network)))
        .collect();
    let stops: HashMap<&Node, Vec<usize>> = nodes
        .iter()
        .map(|&n| {
            let (start, period) = periods.get(n).unwrap();
            (n, find_stops(n, directions, network, *start, *period))
        })
        .collect();
    let (node, (start, max_period)) = periods.iter().max_by_key(|(_n, (_s, p))| *p).unwrap();
    let max_period_stops = stops.get(node).unwrap();
    let mut pos = *start;
    loop {
        for stop in max_period_stops {
            let inner_pos = pos + stop;
            if nodes.iter().all(|&n| {
                let (start, period) = periods.get(n).unwrap();
                let stops = stops.get(n).unwrap();
                stops.contains(&((inner_pos - start) % period))
            }) {
                return inner_pos;
            }
        }
        pos += max_period;
    }
}

fn find_period(node: &Node, directions: &[Direction], network: &Network) -> (usize, usize) {
    let mut node = node;
    let mut visited: HashMap<(&Node, usize), usize> = HashMap::new();
    for (i, (j, direction)) in repeat(directions)
        .flat_map(|d| d.iter().enumerate())
        .enumerate()
    {
        let key = (node, j);
        if let Some(previous_i) = visited.get(&key) {
            return (*previous_i, i - previous_i);
        }
        visited.insert(key, i);
        let (left, right) = network.get(node).unwrap();
        node = match direction {
            Direction::L => left,
            Direction::R => right,
        };
    }
    unreachable!();
}

fn find_stops(
    node: &Node,
    directions: &[Direction],
    network: &Network,
    start: usize,
    period: usize,
) -> Vec<usize> {
    let mut node = node;
    let mut stops = Vec::new();
    for (i, direction) in repeat(directions).flatten().enumerate() {
        if i > start + period {
            break;
        }
        if i >= start && node.ends_with('Z') {
            stops.push(i - start);
        }
        let (left, right) = network.get(node).unwrap();
        node = match direction {
            Direction::L => left,
            Direction::R => right,
        };
    }
    stops
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_periods() -> Result<(), Error> {
        let input = "LR\n\
                     \n\
                     11A = (11B, XXX)\n\
                     11B = (XXX, 11Z)\n\
                     11Z = (11B, XXX)\n\
                     22A = (22B, XXX)\n\
                     22B = (22C, 22C)\n\
                     22C = (22Z, 22Z)\n\
                     22Z = (22B, 22B)\n\
                     XXX = (XXX, XXX)";
        let (directions, network) = parse(input, puzzle_input)?;
        let node = "11A".to_string();
        // 11A, 11B, 11Z, 11B, ...
        //      ^ start
        let (start, period) = find_period(&node, &directions, &network);
        assert_eq!(start, 1);
        assert_eq!(period, 2);
        let stops = find_stops(&node, &directions, &network, start, period);
        assert_eq!(stops, vec![1]);
        Ok(())
    }

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "LR\n\
                     \n\
                     11A = (11B, XXX)\n\
                     11B = (XXX, 11Z)\n\
                     11Z = (11B, XXX)\n\
                     22A = (22B, XXX)\n\
                     22B = (22C, 22C)\n\
                     22C = (22Z, 22Z)\n\
                     22Z = (22B, 22B)\n\
                     XXX = (XXX, XXX)";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 6);
        Ok(())
    }
}
