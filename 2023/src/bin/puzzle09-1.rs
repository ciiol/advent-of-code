// https://adventofcode.com/2023/day/9
// You pull out your handy Oasis And Sand Instability Sensor and analyze your surroundings.
// The OASIS produces a report of many values and how they are changing over time
// (your puzzle input). Each line in the report contains the history of a single value.
//
// To best protect the oasis, your environmental report should include a prediction of
// the next value in each history. To do this, start by making a new sequence from
// the difference at each step of your history. If that sequence is not all zeroes,
// repeat this process, using the sequence you just generated as the input sequence.
// Once all of the values in your latest sequence are zeroes, you can extrapolate what
// the next value of the original history should be.
//
// Analyze your OASIS report and extrapolate the next value for each history.
// What is the sum of these extrapolated values?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::character::complete::{i64 as i64_parser, newline, space1};
use nom::multi::separated_list1;
use nom::IResult;

use advent::runner::run_puzzle;

type Measurement = i64;
type History = Vec<Measurement>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle09.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<History>> {
    let history = separated_list1(space1, i64_parser);
    separated_list1(newline, history)(input)
}

fn puzzle(input: &[History]) -> i64 {
    input.iter().map(|h| predict(h)).sum()
}

fn predict(history: &[Measurement]) -> Measurement {
    let mut histories = vec![history.to_vec()];
    loop {
        let history = histories.last().unwrap();
        if history.iter().all(|&v| v == 0) {
            break;
        }
        let next_history = history.windows(2).map(|w| w[1] - w[0]).collect();
        histories.push(next_history);
    }
    histories.iter().map(|h| h.last().unwrap()).sum()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "0 3 6 9 12 15\n\
                     1 3 6 10 15 21\n\
                     10 13 16 21 30 45";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 114);
        Ok(())
    }
}
