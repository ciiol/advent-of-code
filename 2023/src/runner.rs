use std::borrow::Borrow;
use std::fmt::Display;
use std::fs::File;
use std::io::{Error, Read};

use nom::bytes::complete::tag;
use nom::combinator::opt;
use nom::sequence::terminated;
use nom::Parser;

use crate::parser::parse;

pub fn run_puzzle<TP, TF, P, F, R>(file_name: &str, parser: P, solver: F) -> Result<(), Error>
where
    P: for<'a> Parser<&'a str, TP, nom::error::Error<&'a str>>,
    F: Fn(&TF) -> R,
    TP: Borrow<TF>,
    TF: ?Sized,
    R: Display,
{
    let mut input_file = File::open(file_name)?;
    let mut input_str = String::new();
    input_file.read_to_string(&mut input_str)?;
    let input = parse(input_str.as_str(), terminated(parser, opt(tag("\n"))))?;
    println!("{}", solver(input.borrow()));
    Ok(())
}
