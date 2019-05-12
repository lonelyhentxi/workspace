use clap::ArgMatches;
use cparser::grammars::gen_cgrammars;
use clr1::{IToken, Parser};
use serde_json;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::process::exit;

pub fn parse_command(matches: &ArgMatches) {
    let tokens = matches.value_of("tokens").unwrap();
    let sequence = matches.value_of("sequence").unwrap();
    let table = matches.value_of("table").unwrap();
    let mut tokens_file = File::open(tokens).unwrap_or_else(|e| {
        writeln!(std::io::stderr(), "Tokens file error: {}", e).ok();
        exit(-1);
    });
    let mut _sequence_file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(sequence)
        .unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "Sequence file error: {}", e).ok();
            exit(-1);
        });
    let mut table_file = File::open(table).unwrap_or_else(|e| {
        writeln!(std::io::stderr(), "Table file error: {}", e).ok();
        exit(-1);
    });
    let tokens = {
        let mut tokens_string = String::new();
        tokens_file
            .read_to_string(&mut tokens_string)
            .unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "{}", e).ok();
                exit(-1);
            });
        let mut table_string = String::new();
        table_file
            .read_to_string(&mut table_string)
            .unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "{}", e).ok();
                exit(-1);
            });
        let table_json: (HashMap<usize, (String, String)>, HashMap<usize, (String,)>) =
            serde_json::from_str(&table_string).unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "{}", e).ok();
                exit(-1);
            });
        let tokens_json: Vec<(usize, String, Option<usize>)> = serde_json::from_str(&tokens_string)
            .unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "{}", e).ok();
                exit(-1);
            });
        tokens_json
            .into_iter()
            .map(|(x, y, _)| (x, y))
            .map(|(i, c)| (table_json.0.get(&i).unwrap().0.clone(), c))
            .map(|x| Box::new(x) as Box<IToken>)
            .collect::<Vec<_>>()
    };
    {
        let grammar = gen_cgrammars();
        let parser = Parser::new(grammar);
        let res = parser.parse(tokens.into_iter()).unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "{}", e).ok();
            exit(-1);
        });
        res.print();
    }
}
