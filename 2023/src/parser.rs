use std::fmt::{Debug, Display};
use std::io::{Error, ErrorKind};
use std::ops::RangeFrom;

use nom::character::complete::newline;
use nom::combinator::map;
use nom::error::ParseError;
use nom::multi::{many1, separated_list1};
use nom::{Finish, InputIter, InputLength, Parser, Slice};

use crate::matrix::Matrix;

/// # Errors
/// Returns an error if the parser fails or if the input is not fully consumed.
pub fn parse<I, T, E, F>(val: I, mut parser: F) -> Result<T, Error>
where
    F: Parser<I, T, E>,
    E: Display,
    I: InputLength + Debug,
{
    match parser.parse(val).finish() {
        Ok((remaining, result)) => {
            if remaining.input_len() == 0 {
                Ok(result)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("Input not fully consumed: {remaining:?}"),
                ))
            }
        }
        Err(err) => Err(Error::new(ErrorKind::InvalidData, format!("{err}"))),
    }
}

pub fn matrix<I, E: ParseError<I>, F, T>(element: F) -> impl Parser<I, Matrix<T>, E>
where
    F: Parser<I, T, E>,
    I: Slice<RangeFrom<usize>> + InputLength + Clone + InputIter<Item = char>,
{
    let row = many1(element);
    let data = separated_list1(newline, row);
    map(data, Matrix::new)
}
