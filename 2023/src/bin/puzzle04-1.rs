// https://adventofcode.com/2023/day/4
// Each card has two lists of numbers separated by a vertical bar (|): a
// list of winning numbers and then a list of numbers you have. You organize
// the information into a table (your puzzle input).
//
// As far as the Elf has been able to figure out, you have to figure out which of
// the numbers you have appear in the list of winning numbers. The first match makes
// the card worth one point and each match after the first doubles the point value of that card.
//
// How many points are they worth in total?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::bytes::complete::tag;
use nom::character::complete::{newline, space0, space1, u32 as u32_parser};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};

use advent::runner::run_puzzle;

type Card = (Vec<u32>, Vec<u32>);

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle04.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Card>> {
    let card_header = terminated(
        preceded(terminated(tag("Card"), space1), u32_parser),
        delimited(space0, tag(":"), space1),
    );
    let own_numbers = separated_list1(space1, u32_parser);
    let win_numbers = separated_list1(space1, u32_parser);
    let separator = delimited(space1, tag("|"), space1);
    let codes = separated_pair(win_numbers, separator, own_numbers);
    let card = preceded(card_header, codes);
    separated_list1(newline, card).parse(input)
}

fn puzzle(input: &[Card]) -> u64 {
    input
        .iter()
        .map(|(win_numbers, own_numbers)| {
            own_numbers
                .iter()
                .filter(|&n| win_numbers.contains(n))
                .count()
        })
        .map(|n| {
            if n > 0 {
                2u64.pow((n - 1).try_into().unwrap())
            } else {
                0
            }
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
                     Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
                     Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
                     Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
                     Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
                     Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 13);
        Ok(())
    }
}
