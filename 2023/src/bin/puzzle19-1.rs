// https://adventofcode.com/2023/day/19
// A group of Elves is already here organizing the parts, and they have a system.
//
// To start, each part is rated in each of four categories:
//  - x: Extremely cool looking
//  - m: Musical (it makes a noise when you hit it)
//  - a: Aerodynamic
//  - s: Shiny
//
// Then, each part is sent through a series of workflows that will ultimately accept or reject the part.
// Each workflow has a name and contains a list of rules; each rule specifies a condition and where
// to send the part if the condition is true. The first rule that matches the part being considered is
// applied immediately, and the part moves on to the destination described by the rule.
// (The last rule in each workflow has no condition and always applies if reached.)
//
// Sort through all of the parts you've been given; what do you get if you add together all
// of the rating numbers for all of the parts that ultimately get accepted?

extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::newline;
use nom::character::complete::{alpha1, i64 as i64_parser};
use nom::combinator::{map, opt, value};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, separated_pair, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Part {
    x: i64,
    m: i64,
    a: i64,
    s: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Decision {
    Accept,
    Reject,
    Continue(WorkflowName),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Condition {
    GreaterThan(i64),
    LessThan(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Rule {
    pub x: Option<Condition>,
    pub m: Option<Condition>,
    pub a: Option<Condition>,
    pub s: Option<Condition>,
    pub decision: Decision,
}

type WorkflowName = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Workflow {
    pub name: String,
    pub rules: Vec<Rule>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct System {
    workflows: Vec<Workflow>,
    by_name: HashMap<WorkflowName, usize>,
}

impl System {
    pub fn new(workflows: Vec<Workflow>) -> Self {
        let by_name = workflows
            .iter()
            .enumerate()
            .map(|(i, workflow)| (workflow.name.clone(), i))
            .collect();
        Self { workflows, by_name }
    }

    pub fn apply(&self, part: &Part) -> &Decision {
        let mut workflow = &self.workflows[self.by_name["in"]];
        loop {
            let decision = workflow.apply(part);
            match decision {
                Decision::Accept | Decision::Reject => return decision,
                Decision::Continue(name) => workflow = &self.workflows[self.by_name[name]],
            }
        }
    }
}

impl Condition {
    pub fn check(&self, value: i64) -> bool {
        match self {
            Self::GreaterThan(threshold) => value > *threshold,
            Self::LessThan(threshold) => value < *threshold,
        }
    }
}

impl Workflow {
    pub fn apply(&self, part: &Part) -> &Decision {
        self.rules.iter().find_map(|rule| rule.apply(part)).unwrap()
    }
}

impl Rule {
    pub fn apply(&self, part: &Part) -> Option<&Decision> {
        let x = self.x.as_ref().map_or(true, |c| c.check(part.x));
        let m = self.m.as_ref().map_or(true, |c| c.check(part.m));
        let a = self.a.as_ref().map_or(true, |c| c.check(part.a));
        let s = self.s.as_ref().map_or(true, |c| c.check(part.s));
        (x && m && a && s).then_some(&self.decision)
    }
}

impl Part {
    pub fn rating(&self) -> i64 {
        self.x + self.m + self.a + self.s
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle19.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, (System, Vec<Part>)> {
    let decision = alt((
        value(Decision::Accept, tag("A")),
        value(Decision::Reject, tag("R")),
        map(workflow_name, Decision::Continue),
    ));
    let condition = alt((
        map(preceded(tag(">"), i64_parser), Condition::GreaterThan),
        map(preceded(tag("<"), i64_parser), Condition::LessThan),
    ));
    let rule = map(
        tuple((opt(tuple((alpha1, condition, tag(":")))), decision)),
        |(condition_block, decision)| {
            let mut rule = Rule {
                x: None,
                m: None,
                a: None,
                s: None,
                decision,
            };
            match condition_block {
                Some(("x", condition, _)) => rule.x = Some(condition),
                Some(("m", condition, _)) => rule.m = Some(condition),
                Some(("a", condition, _)) => rule.a = Some(condition),
                Some(("s", condition, _)) => rule.s = Some(condition),
                None => {}
                _ => panic!("Unexpected condition: {condition_block:?}"),
            }
            rule
        },
    );
    let workflow = map(
        tuple((
            workflow_name,
            tag("{"),
            separated_list1(tag(","), rule),
            tag("}"),
        )),
        |(name, _, rules, _)| Workflow { name, rules },
    );
    let system = map(separated_list1(newline, workflow), System::new);
    let part = map(
        delimited(
            tag("{"),
            tuple((
                delimited(tag("x="), i64_parser, tag(",")),
                delimited(tag("m="), i64_parser, tag(",")),
                delimited(tag("a="), i64_parser, tag(",")),
                preceded(tag("s="), i64_parser),
            )),
            tag("}"),
        ),
        |(x, m, a, s)| Part { x, m, a, s },
    );
    let parts = separated_list1(newline, part);
    separated_pair(system, tag("\n\n"), parts)(input)
}

fn workflow_name(input: &str) -> IResult<&str, WorkflowName> {
    map(alpha1, |s: &str| s.to_string())(input)
}

fn puzzle(input: &(System, Vec<Part>)) -> i64 {
    let (system, parts) = input;
    parts
        .iter()
        .filter(|part| system.apply(part) == &Decision::Accept)
        .map(Part::rating)
        .sum()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "px{a<2006:qkq,m>2090:A,rfg}\n\
                     pv{a>1716:R,A}\n\
                     lnx{m>1548:A,A}\n\
                     rfg{s<537:gd,x>2440:R,A}\n\
                     qs{s>3448:A,lnx}\n\
                     qkq{x<1416:A,crn}\n\
                     crn{x>2662:A,R}\n\
                     in{s<1351:px,qqz}\n\
                     qqz{s>2770:qs,m<1801:hdj,R}\n\
                     gd{a>3333:R,R}\n\
                     hdj{m>838:A,pv}\n\
                     \n\
                     {x=787,m=2655,a=1222,s=2876}\n\
                     {x=1679,m=44,a=2067,s=496}\n\
                     {x=2036,m=264,a=79,s=2244}\n\
                     {x=2461,m=1339,a=466,s=291}\n\
                     {x=2127,m=1623,a=2188,s=1013}";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 19114);
        Ok(())
    }
}
