// https://adventofcode.com/2023/day/1#part2
// The calibration document (your puzzle input) has been amended by a very young
// Elf who was apparently just excited to show off her art skills. Consequently,
// the Elves are having trouble reading the values on the document.
//
// The newly-improved calibration document consists of lines of text;
// each line originally contained a specific calibration value that the
// Elves now need to recover. On each line, the calibration value can be
// found by combining the first digit and the last digit (in that order)
// to form a single two-digit number.
//
// Your calculation isn't quite right. It looks like some of the digits are actually
// spelled out with letters: one, two, three, four, five, six, seven, eight,
// and nine also count as valid "digits".

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::satisfy;
use nom::combinator::{all_consuming, map, peek, value};
use nom::error::ParseError;
use nom::multi::many1;
use nom::{Finish, IResult, InputLength};
use std::fmt::Display;
use std::fs::File;
use std::io::{Error, ErrorKind, Read};

#[derive(Debug, Clone, PartialEq)]
enum Item {
    Digit(u32),
    Letter(char),
}

impl Item {
    fn into_digit(self) -> Option<u32> {
        match self {
            Item::Digit(n) => Some(n),
            Item::Letter(_) => None,
        }
    }
}

fn main() -> Result<(), Error> {
    let mut input_file = File::open("inputs/puzzle01.txt").unwrap();
    let mut input = String::new();
    input_file.read_to_string(&mut input).unwrap();
    println!("{}", puzzle(&input)?);
    Ok(())
}

fn puzzle(input: &str) -> Result<u32, Error> {
    let non_empty = |s: &&str| !s.is_empty();
    input.lines().filter(non_empty).map(calibration).sum()
}

fn calibration(line: &str) -> Result<u32, Error> {
    let mut iter = parse(line, items1)?
        .into_iter()
        .filter_map(Item::into_digit);
    let first = iter.next().unwrap();
    let last = iter.last().unwrap_or(first);
    Ok(first * 10 + last)
}

fn items1(input: &str) -> IResult<&str, Vec<Item>> {
    many1(item)(input)
}

fn item(input: &str) -> IResult<&str, Item> {
    let is_digit = |c: char| c.is_ascii_digit();
    let is_letter = |c: char| c.is_alphabetic();
    let digit = map(satisfy(is_digit), |c: char| {
        Item::Digit(c.to_digit(10).unwrap())
    });
    let one = value(Item::Digit(1), tag("one"));
    let two = value(Item::Digit(2), tag("two"));
    let three = value(Item::Digit(3), tag("three"));
    let four = value(Item::Digit(4), tag("four"));
    let five = value(Item::Digit(5), tag("five"));
    let six = value(Item::Digit(6), tag("six"));
    let seven = value(Item::Digit(7), tag("seven"));
    let eight = value(Item::Digit(8), tag("eight"));
    let nine = value(Item::Digit(9), tag("nine"));
    let letter = map(satisfy(is_letter), Item::Letter);
    let parser = alt((
        digit, one, two, three, four, five, six, seven, eight, nine, letter,
    ));
    let (input, result) = peek(parser)(input)?;
    let (rest, _) = take(1usize)(input)?;
    Ok((rest, result))
}

fn parse<I, T, E: ParseError<I>, F>(val: I, parser: F) -> Result<T, Error>
where
    F: Fn(I) -> IResult<I, T, E>,
    E: Display,
    I: InputLength,
{
    match all_consuming(parser)(val).finish() {
        Ok((_remaining, result)) => Ok(result),
        Err(err) => Err(Error::new(ErrorKind::Other, format!("{err}"))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "two1nine\n\
                     eightwothree\n\
                     abcone2threexyz\n\
                     xtwone3four\n\
                     4nineeightseven2\n\
                     zoneight234\n\
                     7pqrstsixteen";
        assert_eq!(puzzle(input)?, 281);
        Ok(())
    }

    #[test]
    fn test_overlapping() -> Result<(), Error> {
        assert_eq!(calibration("twoneighthree")?, 23);
        Ok(())
    }
}
