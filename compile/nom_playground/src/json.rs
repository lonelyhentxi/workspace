use indexmap::IndexMap;
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{is_not, tag, take, take_while},
    character::complete::char as character,
    combinator::{cut, map, map_opt, map_res, value, verify},
    error::context,
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, preceded, separated_pair, terminated},
};

#[derive(Debug, PartialEq, Clone)]
pub enum JsonValue {
    Object(IndexMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

pub fn parse_null_node(json: &str) -> IResult<&str, JsonValue> {
    value(JsonValue::Null, tag("null")).parse(json)
}

fn parse_true(json: &str) -> IResult<&str, bool> {
    value(true, tag("true")).parse(json)
}

fn parse_false(json: &str) -> IResult<&str, bool> {
    value(false, tag("false")).parse(json)
}

pub fn parse_boolean(json: &str) -> IResult<&str, bool> {
    alt((parse_true, parse_false)).parse(json)
}

fn parse_u16_hex(json: &str) -> IResult<&str, u16> {
    map_res(take(4usize), |s| u16::from_str_radix(s, 16)).parse(json)
}

pub fn parse_unicode(json: &str) -> IResult<&str, char> {
    map_opt(
        alt((
            map(
                verify(parse_u16_hex, |cp| !(0xD800..0xE000).contains(cp)),
                |cp| cp as u32,
            ),
            map(
                verify(
                    separated_pair(parse_u16_hex, tag("\\u"), parse_u16_hex),
                    |(hi, lo)| (0xD800..0xDC00).contains(hi) && (0xDC00..0xE000).contains(lo),
                ),
                |(hi, lo)| {
                    let high_ten = (hi as u32) - 0xD800;
                    let low_ten = (lo as u32) - 0xDC00;
                    (high_ten << 10) + low_ten + 0x10000
                },
            ),
        )),
        std::char::from_u32,
    )
    .parse(json)
}

pub fn parse_escaped_char(json: &str) -> IResult<&str, char> {
    preceded(
        character('\\'),
        alt((
            preceded(character('u'), parse_unicode),
            value('"', character('"')),
            value('\\', character('\\')),
            value('/', character('/')),
            value('\u{08}', character('b')),
            value('\u{0C}', character('f')),
            value('\n', character('n')),
            value('\r', character('r')),
            value('\t', character('t')),
        )),
    )
    .parse(json)
}

fn parse_unescaped_char(json: &str) -> IResult<&str, &str> {
    is_not("\"\\\x00-\x1F").parse(json)
}

enum StringFragment<'a> {
    Str(&'a str),
    Char(char),
}

fn parse_string(json: &str) -> IResult<&str, String> {
    delimited(
        character('"'),
        map(
            many0(alt((
                map(parse_unescaped_char, StringFragment::Str),
                map(parse_escaped_char, StringFragment::Char),
            ))),
            |fragments| {
                let mut ns = String::new();
                for frag in fragments {
                    match frag {
                        StringFragment::Char(c) => {
                            ns.push(c);
                        }
                        StringFragment::Str(s) => {
                            ns.push_str(s);
                        }
                    }
                }
                ns
            },
        ),
        character('"'),
    )
    .parse(json)
}

const SPACE_SET: &str = "\t\r\n ";

fn parse_spaces(json: &str) -> IResult<&str, &str> {
    take_while(|c| SPACE_SET.contains(c)).parse(json)
}

fn parse_key_value(json: &str) -> IResult<&str, (String, JsonValue)> {
    delimited(
        parse_spaces,
        separated_pair(
            preceded(parse_spaces, parse_string),
            cut(delimited(parse_spaces, character(':'), parse_spaces)),
            parse_json,
        ),
        parse_spaces,
    )
    .parse(json)
}

pub fn parse_object(json: &str) -> IResult<&str, IndexMap<String, JsonValue>> {
    context(
        "object",
        preceded(
            character('{'),
            cut(terminated(
                delimited(
                    parse_spaces,
                    map(
                        separated_list0(
                            delimited(parse_spaces, character(','), parse_spaces),
                            parse_key_value,
                        ),
                        |key_value_list| key_value_list.into_iter().collect::<IndexMap<_, _>>(),
                    ),
                    parse_spaces,
                ),
                character('}'),
            )),
        ),
    )
    .parse(json)
}

pub fn parse_array(json: &str) -> IResult<&str, Vec<JsonValue>> {
    context(
        "array",
        preceded(
            character('['),
            cut(terminated(
                delimited(
                    parse_spaces,
                    separated_list0(
                        delimited(parse_spaces, character(','), parse_spaces),
                        parse_json,
                    ),
                    parse_spaces,
                ),
                character(']'),
            )),
        ),
    )
    .parse(json)
}

pub fn parse_number(json: &str) -> IResult<&str, f64> {
    double(json)
}

pub fn parse_null(json: &str) -> IResult<&str, ()> {
    value((), tag("null")).parse(json)
}

fn parse_json(json: &str) -> IResult<&str, JsonValue> {
    delimited(
        parse_spaces,
        alt((
            map(parse_array, JsonValue::Array),
            map(parse_object, JsonValue::Object),
            map(parse_string, JsonValue::String),
            map(parse_boolean, JsonValue::Boolean),
            map(parse_number, JsonValue::Number),
            map(parse_null, |_| JsonValue::Null),
        )),
        parse_spaces,
    )
    .parse(json)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_string() {
        assert_eq!(parse_string("\"\""), Ok(("", "".to_string())));
        assert_eq!(parse_string("\"abc\""), Ok(("", "abc".to_string())));
        assert_eq!(
            parse_string("\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0001\\u2014\u{2014}def\""),
            Ok(("", "abc\"\\/\x08\x0C\n\r\t\x01——def".to_string())),
        );
        assert_eq!(
            parse_string("\"\\uD83D\\uDE10\""),
            Ok(("", "😐".to_string()))
        );

        assert!(parse_string("\"").is_err());
        assert!(parse_string("\"abc").is_err());
        assert!(parse_string("\"\\\"").is_err());
        assert!(parse_string("\"\\u123\"").is_err());
        assert!(parse_string("\"\\uD800\"").is_err());
        assert!(parse_string("\"\\uD800\\uD800\"").is_err());
        assert!(parse_string("\"\\uDC00\"").is_err());
    }

    #[test]
    fn json_object() {
        use JsonValue::*;

        let input = r#"{"a":42,"b":"x"}"#;

        let expected = Object(
            vec![
                ("a".to_string(), Number(42.0)),
                ("b".to_string(), String("x".to_string())),
            ]
            .into_iter()
            .collect(),
        );

        assert_eq!(parse_json(input), Ok(("", expected)));
    }

    #[test]
    fn json_array() {
        use JsonValue::*;

        let input = r#"[42,"x"]"#;

        let expected = Array(vec![Number(42.0), String("x".to_string())]);

        assert_eq!(parse_json(input), Ok(("", expected)));
    }

    #[test]
    fn json_whitespace() {
        use JsonValue::*;

        let input = r#"
          {
            "null" : null,
            "true"  :true ,
            "false":  false  ,
            "number" : 123e4 ,
            "string" : " abc 123 " ,
            "array" : [ false , 1 , "two" ] ,
            "object" : { "a" : 1.0 , "b" : "c" } ,
            "empty_array" : [  ] ,
            "empty_object" : {   }
          }
          "#;

        assert_eq!(
            parse_json(input),
            Ok((
                "",
                Object(
                    vec![
                        ("null".to_string(), Null),
                        ("true".to_string(), Boolean(true)),
                        ("false".to_string(), Boolean(false)),
                        ("number".to_string(), Number(123e4)),
                        ("string".to_string(), String(" abc 123 ".to_string())),
                        (
                            "array".to_string(),
                            Array(vec![Boolean(false), Number(1.0), String("two".to_string())])
                        ),
                        (
                            "object".to_string(),
                            Object(
                                vec![
                                    ("a".to_string(), Number(1.0)),
                                    ("b".to_string(), String("c".to_string())),
                                ]
                                .into_iter()
                                .collect()
                            )
                        ),
                        ("empty_array".to_string(), Array(vec![]),),
                        ("empty_object".to_string(), Object(IndexMap::new()),),
                    ]
                    .into_iter()
                    .collect()
                )
            ))
        );
    }
}
