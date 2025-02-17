#![feature(assert_matches)]
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{char, space1},
    combinator::map,
    sequence::delimited,
};

pub fn tag_sample(json: &str) -> IResult<&str, &str> {
    tag("null")(json)
}

pub fn parse_hello_world(input: &str) -> IResult<&str, ()> {
    let hello = tag("hello");
    let world = tag("world");
    let mut hello_world = (hello, space1, world);

    let result = hello_world.parse(input)?;

    Ok((result.0, ()))
}

pub fn recognize_boolean(input: &str) -> IResult<&str, &str> {
    alt((tag("true"), tag("false"))).parse(input)
}

pub fn recognize_curly_2(input: &str) -> IResult<&str, &str> {
    delimited(char('('), take(2usize), char(')')).parse(input)
}

pub fn parse_boolean(input: &str) -> IResult<&str, bool> {
    alt((
        map(tag("true"), |_| true), 
        map(tag("false"), |_| false)
    )).parse(input)
}

#[cfg(test)]
mod tests {

    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_tag_sample() {
        assert_matches!(tag_sample("null"), Ok(("", "null")));
        assert_matches!(tag_sample("null rest"), Ok((" rest", "null")));
        assert_matches!(tag_sample("not null"), Err(..));
    }

    #[test]
    fn test_parse_hello_world() {
        assert_matches!(parse_hello_world("helloworld"), Err(..));
        assert_matches!(
            parse_hello_world("hello  world   rest"),
            Ok(("   rest", ..))
        );
    }

    #[test]
    fn test_recognize_boolean() {
        assert_matches!(recognize_boolean("true false"), Ok((" false", "true")));
        assert_matches!(recognize_boolean("1true false"), Err(..))
    }

    #[test]
    fn test_curly_2() {
        assert_matches!(recognize_curly_2("(  )  "), Ok(("  ", "  ")));
    }

    #[test]
    fn test_boolean () {
        assert_matches!("true false", Ok((" false", true)));
        assert_matches!("1true false", Err(..));
    }
}
