// https://adventofcode.com/2023/day/12#part2
// The condition records of which springs are damaged (your puzzle input) are damaged.
//
// In the giant field just outside, the springs are arranged into rows. For each row,
// the condition records show every spring and whether it is operational (.) or damaged (#).
// This is the part of the condition records that is itself damaged; for some springs,
// it is simply unknown (?) whether the spring is operational or damaged.
//
// However, the engineer that produced the condition records also duplicated some of this
// information in a different format! After the list of springs for a given row, the size
// of each contiguous group of damaged springs is listed in the order those groups appear
// in the row. This list always accounts for every damaged spring, and each number is the
// entire size of its contiguous group (that is, groups are always separated by at least
// one operational spring
//
// As you look out at the field of springs, you feel like there are way more springs than
// the condition records list. When you examine the records, you discover that they were
// actually folded up this whole time!
//
// To unfold the records, on each row, replace the list of spring conditions with five
// copies of itself (separated by ?) and replace the list of contiguous groups of damaged
// springs with five copies of itself (separated by ,).
//
// Unfold your condition records; what is the new sum of possible arrangement counts?

extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
use std::io::Error;

use itertools::{intersperse, repeat_n};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{newline, space1, u32 as number};
use nom::combinator::{map, value};
use nom::multi::{many1, separated_list1};
use nom::sequence::separated_pair;
use nom::IResult;

use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Spring {
    Broken,
    Operational,
    Unknown,
}

struct Record {
    springs: Vec<Spring>,
    groups: Vec<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct UnfinishedRecord {
    groups: Vec<usize>,
    last: Spring,
}

impl UnfinishedRecord {
    pub fn new(groups: &[usize]) -> Self {
        let mut groups = groups.to_vec();
        groups.reverse();
        Self {
            last: Spring::Operational,
            groups,
        }
    }

    pub fn matches(&self, next: Spring) -> bool {
        let group = self.groups.last();
        match next {
            Spring::Broken => matches!(group, Some(&size) if size > 0),
            Spring::Operational => *group.unwrap_or(&0) == 0 || self.last == Spring::Operational,
            Spring::Unknown => panic!("Unknown spring"),
        }
    }

    pub fn push(&mut self, next: Spring) {
        match next {
            Spring::Broken => {
                let group = self.groups.last_mut().unwrap();
                *group -= 1;
            }
            Spring::Operational => {
                if self.last == Spring::Broken {
                    self.groups.pop().unwrap();
                }
            }
            Spring::Unknown => panic!("Unknown spring"),
        }
        self.last = next;
    }

    pub fn is_finished(&self) -> bool {
        let groups = self.groups.len();
        groups == 0 || (groups == 1 && self.groups[0] == 0)
    }
}

impl Record {
    pub fn new(springs: Vec<Spring>, groups: Vec<usize>) -> Self {
        Self { springs, groups }
    }

    pub fn resolve_unknowns(&self) -> usize {
        let mut records: HashMap<UnfinishedRecord, usize> =
            HashMap::from([(UnfinishedRecord::new(&self.groups), 1)]);
        for &state in &self.springs {
            let mut new_records = HashMap::new();
            if state == Spring::Unknown {
                for (mut record, n) in records {
                    if record.matches(Spring::Broken) && record.matches(Spring::Operational) {
                        let mut cloned = record.clone();
                        cloned.push(Spring::Broken);
                        record.push(Spring::Operational);
                        *new_records.entry(cloned).or_insert(0) += n;
                        *new_records.entry(record).or_insert(0) += n;
                    } else if record.matches(Spring::Broken) {
                        record.push(Spring::Broken);
                        *new_records.entry(record).or_insert(0) += n;
                    } else if record.matches(Spring::Operational) {
                        record.push(Spring::Operational);
                        *new_records.entry(record).or_insert(0) += n;
                    }
                }
            } else {
                for (mut record, n) in records {
                    if record.matches(state) {
                        record.push(state);
                        *new_records.entry(record).or_insert(0) += n;
                    }
                }
            }
            records = new_records;
        }
        records
            .into_iter()
            .filter_map(|(r, n)| if r.is_finished() { Some(n) } else { None })
            .sum()
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle12.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Record>> {
    separated_list1(newline, record)(input)
}

fn record(input: &str) -> IResult<&str, Record> {
    let spring = alt((
        value(Spring::Broken, tag("#")),
        value(Spring::Operational, tag(".")),
        value(Spring::Unknown, tag("?")),
    ));
    let springs = map(many1(spring), |springs| {
        intersperse(repeat_n(springs, 5), vec![Spring::Unknown])
            .flatten()
            .collect()
    });
    let groups = map(separated_list1(tag(","), number), |groups| {
        repeat_n(groups, 5)
            .flatten()
            .map(|g| g.try_into().unwrap())
            .collect()
    });
    map(
        separated_pair(springs, space1, groups),
        |(springs, groups)| Record::new(springs, groups),
    )(input)
}

fn puzzle(input: &[Record]) -> usize {
    input.iter().map(Record::resolve_unknowns).sum()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "???.### 1,1,3\n\
                     .??..??...?##. 1,1,3\n\
                     ?#?#?#?#?#?#?#? 1,3,1,6\n\
                     ????.#...#... 4,1,1\n\
                     ????.######..#####. 1,6,5\n\
                     ?###???????? 3,2,1";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 525_152);
        Ok(())
    }

    #[test]
    fn test_resolve_unknowns() {
        let input = Record::new(vec![Spring::Unknown; 3], vec![3]);
        assert_eq!(input.resolve_unknowns(), 1);
        let input = parse("???.### 1,1,3", record).unwrap();
        assert_eq!(input.resolve_unknowns(), 1);
        let input = parse(".??..??...?##. 1,1,3", record).unwrap();
        assert_eq!(input.resolve_unknowns(), 16384);
        let input = parse("?#?#?#?#?#?#?#? 1,3,1,6", record).unwrap();
        assert_eq!(input.resolve_unknowns(), 1);
        let input = parse("????.#...#... 4,1,1", record).unwrap();
        assert_eq!(input.resolve_unknowns(), 16);
        let input = parse("????.######..#####. 1,6,5", record).unwrap();
        assert_eq!(input.resolve_unknowns(), 2500);
        let input = parse("?###???????? 3,2,1", record).unwrap();
        assert_eq!(input.resolve_unknowns(), 506_250);
    }
}
