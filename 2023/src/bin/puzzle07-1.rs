// https://adventofcode.com/2023/day/7
// In Camel Cards, you get a list of hands, and your goal is to order them based on the strength
// of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3,
// or 2. The relative strength of each card follows this order, where A is the highest and 2
// is the lowest.
//
// Every hand is exactly one type. From strongest to weakest, they are:
// - Five of a kind, where all five cards have the same label: AAAAA
// - Four of a kind, where four cards have the same label and one card has a
//   different label: AA8AA
// - Full house, where three cards have the same label, and the remaining two
//   cards share a different label: 23332
// - Three of a kind, where three cards have the same label, and the remaining two
//   cards are each different from any other card in the hand: TTT98
// - Two pair, where two cards share one label, two other cards share a second label,
//   and the remaining card has a third label: 23432
// - One pair, where two cards share one label, and the other three cards have a different
//   label from the pair and each other: A23A4
// - High card, where all cards' labels are distinct: 23456
// Hands are primarily ordered based on type; for example, every full house is stronger
// than any three of a kind.
//
// If two hands have the same type, a second ordering rule takes effect. Start by comparing the
// first card in each hand. If these cards are different, the hand with the stronger first card
// is considered stronger. If the first card in each hand have the same label, however, then
// move on to considering the second card in each hand. If they differ, the hand with the
// higher second card wins; otherwise, continue with the third card in each hand, then the
// fourth, then the fifth.
//
// To play Camel Cards, you are given a list of hands and their corresponding bid
// (your puzzle input). Each hand wins an amount equal to its bid multiplied by its
// rank, where the weakest hand gets rank 1, the second-weakest hand gets rank 2, and
// so on up to the strongest hand.
//
// Find the rank of every hand in your set. What are the total winnings?
extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{newline, space1, u64 as u64_parser};
use nom::combinator::{map, value};
use nom::multi::{many1, separated_list1};
use nom::sequence::separated_pair;
use nom::IResult;

use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
enum Card {
    A,
    K,
    Q,
    J,
    T,
    N9,
    N8,
    N7,
    N6,
    N5,
    N4,
    N3,
    N2,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
enum Hand {
    FiveOfAKind([Card; 5]),
    FourOfAKind([Card; 5]),
    FullHouse([Card; 5]),
    ThreeOfAKind([Card; 5]),
    TwoPair([Card; 5]),
    OnePair([Card; 5]),
    HighCard([Card; 5]),
}

type Bid = u64;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle07.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<(Hand, Bid)>> {
    let line = separated_pair(hand, space1, u64_parser);
    separated_list1(newline, line)(input)
}

fn puzzle(input: &[(Hand, Bid)]) -> u64 {
    let mut winners: Vec<(Hand, Bid)> = Vec::from(input);
    winners.sort_by(|(hand_a, _), (hand_b, _)| hand_b.cmp(hand_a));
    winners
        .iter()
        .enumerate()
        .map(|(index, (_, bid))| (index + 1) as u64 * bid)
        .sum()
}

fn card(input: &str) -> IResult<&str, Card> {
    alt((
        value(Card::A, tag("A")),
        value(Card::K, tag("K")),
        value(Card::Q, tag("Q")),
        value(Card::J, tag("J")),
        value(Card::T, tag("T")),
        value(Card::N9, tag("9")),
        value(Card::N8, tag("8")),
        value(Card::N7, tag("7")),
        value(Card::N6, tag("6")),
        value(Card::N5, tag("5")),
        value(Card::N4, tag("4")),
        value(Card::N3, tag("3")),
        value(Card::N2, tag("2")),
    ))(input)
}

fn hand(input: &str) -> IResult<&str, Hand> {
    let cards = many1(card);
    map(cards, make_hand)(input)
}

fn make_hand(cards: Vec<Card>) -> Hand {
    let cards: [Card; 5] = cards.try_into().unwrap();
    let card_count: HashMap<Card, usize> = cards.iter().fold(HashMap::new(), |mut acc, card| {
        *acc.entry(*card).or_insert(0) += 1;
        acc
    });
    let mut counts: [usize; 5] = [0; 5];
    for number in card_count.values() {
        counts[*number - 1] += 1;
    }
    match counts {
        [0, 0, 0, 0, 1] => Hand::FiveOfAKind(cards),
        [1, 0, 0, 1, 0] => Hand::FourOfAKind(cards),
        [0, 1, 1, 0, 0] => Hand::FullHouse(cards),
        [2, 0, 1, 0, 0] => Hand::ThreeOfAKind(cards),
        [1, 2, 0, 0, 0] => Hand::TwoPair(cards),
        [3, 1, 0, 0, 0] => Hand::OnePair(cards),
        [5, 0, 0, 0, 0] => Hand::HighCard(cards),
        _ => unreachable!("Invalid hand: {cards:?}"),
    }
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "32T3K 765\n\
                     T55J5 684\n\
                     KK677 28\n\
                     KTJJT 220\n\
                     QQQJA 483";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 6440);
        Ok(())
    }
}
