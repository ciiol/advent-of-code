// https://adventofcode.com/2023/day/1
// The calibration document (your puzzle input) has been amended by a very young
// Elf who was apparently just excited to show off her art skills. Consequently,
// the Elves are having trouble reading the values on the document.
//
// The newly-improved calibration document consists of lines of text;
// each line originally contained a specific calibration value that the
// Elves now need to recover. On each line, the calibration value can be
// found by combining the first digit and the last digit (in that order)
// to form a single two-digit number.

use std::fs::File;
use std::io::Read;

fn main() {
    let mut input_file = File::open("inputs/puzzle01.txt").unwrap();
    let mut input = String::new();
    input_file.read_to_string(&mut input).unwrap();
    println!("{}", puzzle(&input));
}

fn puzzle(input: &str) -> u32 {
    let mut sum = 0;
    for line in input.lines() {
        let mut iter = line.chars().filter(char::is_ascii_digit);
        let first = iter.next().unwrap().to_digit(10).unwrap();
        let last = iter.last().map_or(first, |c| c.to_digit(10).unwrap());
        sum += first * 10 + last;
    }
    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_puzzle() {
        let input = "1abc2\n\
                     pqr3stu8vwx\n\
                     a1b2c3d4e5f\n\
                     treb7uchet";
        assert_eq!(puzzle(input), 142);
    }
}
