// https://adventofcode.com/2023/day/15#part2
// The HASH algorithm is:
// - Determine the ASCII code for the current character of the string.
// - Increase the current value by the ASCII code you just determined.
// - Set the current value to itself multiplied by 17.
// - Set the current value to the remainder of dividing itself by 256.
//
// The initialization sequence (your puzzle input) is a comma-separated list of steps to start the
// Lava Production Facility. The book goes on to explain how to perform each step in the initialization sequence.
// ...
// What is the focusing power of the resulting lens configuration?

extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, one_of, u8 as u8_parser};
use nom::combinator::map;
use nom::multi::{many1, separated_list1};
use nom::sequence::tuple;
use nom::IResult;

use advent::runner::run_puzzle;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Lens {
    pub focal_length: u8,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Command {
    Remove(String),
    Insert(Lens),
}

type Box = Vec<Lens>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle15.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Command>> {
    let insert_command = map(
        tuple((alpha1, tag("="), u8_parser)),
        |(label, _sign, number): (&str, _, u8)| {
            let label = label.to_string();
            Command::Insert(Lens {
                focal_length: number,
                label,
            })
        },
    );
    let remove_command = map(tuple((alpha1, tag("-"))), |(label, _sign): (&str, _)| {
        let label = label.to_string();
        Command::Remove(label)
    });
    let command = alt((insert_command, remove_command));
    let separator = many1(one_of(",\n"));
    separated_list1(separator, command)(input)
}

fn puzzle(input: &[Command]) -> usize {
    let boxes: Vec<Box> = vec![vec![]; 256];
    let boxes = input.iter().fold(boxes, apply_command);
    boxes
        .into_iter()
        .enumerate()
        .map(|(i, b)| {
            let box_number = i + 1;
            b.into_iter()
                .enumerate()
                .map(move |(i, lens)| {
                    let slot = i + 1;
                    lens.focal_length as usize * box_number * slot
                })
                .sum::<usize>()
        })
        .sum()
}

fn apply_command(boxes: Vec<Box>, command: &Command) -> Vec<Box> {
    let mut new_boxes = boxes;
    match command {
        Command::Insert(lens) => {
            let hash = hash(&lens.label);
            let b = &mut new_boxes[hash];
            if let Some(i) = b.iter().position(|l| l.label == lens.label) {
                b[i] = lens.clone();
            } else {
                b.push(lens.clone());
            }
        }
        Command::Remove(label) => {
            let hash = hash(label);
            let b = &mut new_boxes[hash];
            let indexes: Vec<_> = b
                .iter()
                .enumerate()
                .filter_map(|(i, l)| if l.label == *label { Some(i) } else { None })
                .collect();
            for i in indexes {
                b.remove(i);
            }
        }
    }
    new_boxes
}

fn hash(input: &str) -> usize {
    let mut current = 0;
    for c in input.chars() {
        let code = c as usize;
        current += code;
        current *= 17;
        current %= 256;
    }
    current
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 145);
        Ok(())
    }
}
