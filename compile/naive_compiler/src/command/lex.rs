use clap::ArgMatches;
use clex::core::LexTable;
use clex::lex::lex;
use clex::pattern::gen_patterns;
use serde_json;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::iter::FromIterator;
use std::process::exit;

fn get_ident_table(
    type_table: &HashMap<usize, (String, String)>,
    tokens: &[(usize, String)],
) -> (HashMap<usize, (String,)>, HashMap<String, usize>) {
    let mut ident_table_rev: HashMap<String, usize> = HashMap::new();
    for (index, token) in tokens {
        if type_table.get(index).unwrap().0 == "IDENTIFIER" && !ident_table_rev.contains_key(token)
        {
            ident_table_rev.insert(token.to_string(), ident_table_rev.len());
        }
    }
    let ident_table: HashMap<usize, (String,)> =
        HashMap::from_iter(ident_table_rev.iter().map(|(k, v)| (*v, (k.to_string(),))));
    (ident_table, ident_table_rev)
}

pub fn lex_command(matches: &ArgMatches) {
    let input = matches.value_of("input").unwrap();
    let output = matches.value_of("output").unwrap();
    let table = matches.value_of("table").unwrap();
    let mut input_file = File::open(input).unwrap_or_else(|e| {
        writeln!(std::io::stderr(), "Input file error: {}", e).ok();
        exit(-1);
    });
    let mut output_file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(output)
        .unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "Token file error: {}", e).ok();
            exit(-1);
        });
    let mut table_file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(table)
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
    let type_table = lex_table
        .0
        .values()
        .map(|(index, type_name, super_name)| (*index, (type_name.clone(), super_name.clone())))
        .collect::<HashMap<usize, (String, String)>>();
    match lex(lex_patterns, &lex_table, &sstream) {
        Ok(ref tokens) => {
            let (ident_table, ident_table_rev) = get_ident_table(&type_table, tokens);
            {
                // write tbstream
                let tstream = serde_json::to_string_pretty(
                    &tokens
                        .iter()
                        .filter(|(index, _)| {
                            let typeinfo = type_table.get(index).unwrap();
                            typeinfo.1 != "SPACE"
                        })
                        .map(|(index, content)| {
                            (*index, content.to_string(), ident_table_rev.get(content))
                        })
                        .collect::<Vec<(usize, String, Option<&usize>)>>(),
                )
                .unwrap_or_else(|e| {
                    writeln!(std::io::stderr(), "{}", e).ok();
                    exit(-1);
                });
                output_file
                    .write(&tstream.into_bytes())
                    .unwrap_or_else(|e| {
                        writeln!(std::io::stderr(), "{}", e).ok();
                        exit(-1);
                    });
            }
            {
                let type_table = type_table
                    .into_iter()
                    .filter(|(_, (_, super_type))| super_type != "SPACE")
                    .collect::<HashMap<usize, (String, String)>>();
                let tbstream = serde_json::to_string_pretty(&(type_table, ident_table))
                    .unwrap_or_else(|e| {
                        writeln!(std::io::stderr(), "{}", e).ok();
                        exit(-1);
                    });
                table_file
                    .write(&tbstream.into_bytes())
                    .unwrap_or_else(|e| {
                        writeln!(std::io::stderr(), "{}", e).ok();
                        exit(-1);
                    });
            }
        }
        Err(content) => {
            writeln!(
                std::io::stderr(),
                "There are syntax error in your source file. Content there are {:?}",
                &content
            )
            .ok();
            exit(-1);
        }
    }
}
