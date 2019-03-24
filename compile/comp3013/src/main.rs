use clex::lex::lex;
use clap::{Arg, App, SubCommand};
use std::fs::{File, OpenOptions};
use std::process::exit;
use std::io::{Read, Write};
use serde_json;
use clex::pattern::gen_patterns;
use std::collections::HashMap;
use std::iter::FromIterator;
use clex::core::LexTable;

fn main() {
    let matches = App::new("[A Progressive Naive Compiler]")
        .version("0.1.0")
        .author("lonelyhentai <master@evernightfireworks.com>")
        .about("do some naive compile.")
        .subcommand(SubCommand::with_name("lex")
            .about("Lexical Analysis")
            .arg(Arg::with_name("input")
                .short("i")
                .help("Input source file, should have be pre-processed")
                .value_name("SOURCE_FILE")
                .required(true))
            .arg(Arg::with_name("output")
                .short("o")
                .help("Output token file, json format")
                .value_name("TOKEN_FILE")
                .required(true))
            .arg(Arg::with_name("table")
                .short("t")
                .help("Output table file, json format")
                .value_name("TABLE_FILE")
                .required(true))
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("lex") {
        let input = matches.value_of("input").unwrap();
        let output = matches.value_of("output").unwrap();
        let table = matches.value_of("table").unwrap();
        let mut input_file = File::open(input).unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "Input file error: {}", e).ok();
            exit(-1);
        });
        let mut output_file = OpenOptions::new().write(true).read(true).create(true).open(output)
            .unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "Token file error: {}", e).ok();
                exit(-1);
            });
        let mut table_file = OpenOptions::new().write(true).read(true).create(true).open(table)
            .unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "Table file error: {}", e).ok();
                exit(-1);
            });
        let mut sstream = String::new();
        input_file.read_to_string(&mut sstream).unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "{}", e).ok();
            exit(-1);
        });
        let lex_patterns = gen_patterns();
        let mut lex_table = LexTable::new();
        for pat in &lex_patterns {
            pat.register(&mut lex_table);
        }
        let type_table = lex_table.0
            .values()
            .map(|(index, type_name, super_name)| (*index, (type_name.clone(), super_name.clone())))
            .collect::<HashMap<usize, (String, String)>>();
        match lex(lex_patterns, &lex_table, &sstream) {
            Ok(ref tokens) => {
                let mut ident_table_rev: HashMap<String, usize> = HashMap::new();
                for (index, token) in tokens {
                    if type_table.get(index).unwrap().1 == "identifier" && !ident_table_rev.contains_key(token) {
                        ident_table_rev.insert(token.to_string(), ident_table_rev.len());
                    }
                }
                let ident_table: HashMap<usize, (String, )> =
                    HashMap::from_iter(ident_table_rev.iter().map(|(k, v)| (*v, (k.to_string(), ))));

                let tstream = serde_json::to_string_pretty(&tokens
                    .iter()
                    .map(|(index, content)| {
                        (*index, content.to_string(), ident_table_rev.get(content))
                    })
                    .collect::<Vec<(usize, String, Option<&usize>)>>()
                )
                    .unwrap_or_else(|e| {
                        writeln!(std::io::stderr(), "{}", e).ok();
                        exit(-1);
                    });
                let tbstream = serde_json::to_string_pretty(&(type_table, ident_table))
                    .unwrap_or_else(|e| {
                        writeln!(std::io::stderr(), "{}", e).ok();
                        exit(-1);
                    });
                output_file.write(&tstream.into_bytes()).unwrap_or_else(|e| {
                    writeln!(std::io::stderr(), "{}", e).ok();
                    exit(-1);
                });
                table_file.write(&tbstream.into_bytes()).unwrap_or_else(|e| {
                    writeln!(std::io::stderr(), "{}", e).ok();
                    exit(-1);
                });
            }
            Err(content) => {
                writeln!(std::io::stderr(), "There are syntax error in your source file. Content there are {:?}", &content).ok();
                exit(-1);
            }
        }
    }
}
