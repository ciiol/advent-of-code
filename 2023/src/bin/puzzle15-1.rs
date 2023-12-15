// https://adventofcode.com/2023/day/15
// The HASH algorithm is:
// - Determine the ASCII code for the current character of the string.
// - Increase the current value by the ASCII code you just determined.
// - Set the current value to itself multiplied by 17.
// - Set the current value to the remainder of dividing itself by 256.
//
// The initialization sequence (your puzzle input) is a comma-separated list of steps to start the
// Lava Production Facility. Ignore newline characters when parsing the initialization sequence.
// To verify that your HASH algorithm is working, the book offers the sum of the result of running
// the HASH algorithm on each step in the initialization sequence.
//
// What is the sum of the results?

extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::character::complete::{none_of, one_of};
use nom::combinator::map;
use nom::multi::{many1, separated_list1};
use nom::IResult;

use advent::runner::run_puzzle;

type Command = String;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle15.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Command>> {
    let separator = many1(one_of(",\n"));
    let value = map(many1(none_of(",\n")), |chars| chars.into_iter().collect());
    separated_list1(separator, value)(input)
}

fn puzzle(input: &[Command]) -> u64 {
    input.iter().map(|cmd| hash(cmd)).sum()
}

fn hash(input: &str) -> u64 {
    let mut current = 0;
    for c in input.chars() {
        let code = c as u64;
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
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 1320);
        Ok(())
    }
}
