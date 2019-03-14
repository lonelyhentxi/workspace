use clex::core::lex;
use clap::{Arg, App, SubCommand};
use std::fs::{File, OpenOptions};
use std::process::exit;
use std::io::{Read, Write};

fn main() {
    let matches = App::new("[A Progressive Naive Compiler]")
        .version("0.1.0")
        .author("lonelyhentai <master@evernightfireworks.com>")
        .about("do some naive compile.")
        .subcommand(SubCommand::with_name("lex")
            .about("Lexical Analysis")
            .arg(Arg::with_name("input")
                .short("i")
                .help("input source file")
                .value_name("SOURCE_FILE")
                .required(true))
            .arg(Arg::with_name("output")
                .short("o")
                .help("output target file")
                .value_name("TARGET_FILE")
                .required(true))
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("lex") {
        let input = matches.value_of("input").unwrap();
        let output = matches.value_of("output").unwrap();
        let mut input_file = File::open(input).unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "Input file error: {}", e).ok();
            exit(-1);
        });
        let mut output_file = OpenOptions::new().write(true).read(true).create(true).open(output)
            .unwrap_or_else(|e| {
                writeln!(std::io::stderr(), "Output file error: {}", e).ok();
                exit(-1);
            });
        let mut sstream = String::new();
        input_file.read_to_string(&mut sstream).unwrap_or_else(|e| {
            writeln!(std::io::stderr(), "{}", e).ok();
            exit(-1);
        });
        match lex(&sstream) {
            Some(ref tokens) => {
                let mut tstream = String::new();
                for pair in tokens {
                    if let Some(typename) = &pair.1 {
                        tstream.push_str(&format!("{},{:?},{:?}\n", pair.0, typename, pair.2));
                    };
                }
                output_file.write(&tstream.into_bytes()).unwrap_or_else(|e| {
                    writeln!(std::io::stderr(), "{}", e).ok();
                    exit(-1);
                });
            }
            None => {
                writeln!(std::io::stderr(), "There are syntax error in your source file.").ok();
                exit(-1);
            }
        }
    }
}
