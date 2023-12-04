// https://adventofcode.com/2023/day/4#part2
// Each card has two lists of numbers separated by a vertical bar (|): a
// list of winning numbers and then a list of numbers you have. You organize
// the information into a table (your puzzle input).
//
// There's no such thing as "points". Instead, scratchcards only cause you to win more
// scratchcards equal to the number of winning numbers you have.
// Specifically, you win copies of the scratchcards below the winning card equal to
// the number of matches.
//
// Copies of scratchcards are scored like normal scratchcards and have the same card number
// as the card they copied. So, if you win a copy of card 10 and it has 5 matching numbers,
// it would then win a copy of the same cards that the original card 10 won: cards 11, 12, 13,
// 14, and 15. This process repeats until none of the copies cause you to win any more cards.
// (Cards will never make you copy a card past the end of the table.)
//
// Process all of the original and copied scratchcards until no more scratchcards are won.
// Including the original set of scratchcards, how many total scratchcards do you end up with?
extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
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

fn puzzle(input: &[Card]) -> usize {
    let mut all_copies: HashMap<usize, usize> = HashMap::new();
    let cards = input.iter().enumerate();
    for (i, card) in cards {
        let points = card_points(card);
        let copies = *all_copies.get(&i).unwrap_or(&0);
        for p in 1..=points {
            all_copies
                .entry(i + p)
                .and_modify(|e| *e += copies + 1)
                .or_insert(copies + 1);
        }
    }
    let total_copies: usize = all_copies.values().sum();
    input.len() + total_copies
}

fn card_points(card: &Card) -> usize {
    let (win_numbers, own_numbers) = card;
    own_numbers
        .iter()
        .filter(|&n| win_numbers.contains(n))
        .count()
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
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 30);
        Ok(())
    }
}
