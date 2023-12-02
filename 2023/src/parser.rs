use std::fmt::Display;
use std::io::{Error, ErrorKind};

use nom::combinator::all_consuming;
use nom::error::ParseError;
use nom::{Finish, IResult, InputLength};

/// # Errors
/// Returns an error if the parser fails or if the input is not fully consumed.
pub fn parse<I, T, E: ParseError<I>, F>(val: I, parser: F) -> Result<T, Error>
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
