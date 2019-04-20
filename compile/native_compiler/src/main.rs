use clap::{App, Arg, SubCommand};
use naive_compiler::command::{lex_command, parse_command};

fn main() {
    let matches = App::new("[A Progressive Naive Compiler]")
        .version("0.1.0")
        .author("lonelyhentai <master@evernightfireworks.com>")
        .about("do some naive compile.")
        .subcommand(
            SubCommand::with_name("lex")
                .about("Lexical Analysis")
                .arg(
                    Arg::with_name("input")
                        .short("i")
                        .help("Input source file, should have be pre-processed")
                        .value_name("SOURCE_FILE")
                        .required(true),
                )
                .arg(
                    Arg::with_name("output")
                        .short("o")
                        .help("Output token file, json format")
                        .value_name("TOKEN_FILE")
                        .required(true),
                )
                .arg(
                    Arg::with_name("table")
                        .short("t")
                        .help("Output table file, json format")
                        .value_name("TABLE_FILE")
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("parse")
                .about("Grammar Analysis")
                .arg(
                    Arg::with_name("table")
                        .short("t")
                        .help("Input table file")
                        .value_name("TABLE_FILE")
                        .required(true),
                )
                .arg(
                    Arg::with_name("tokens")
                        .short("i")
                        .help("Input tokens file")
                        .value_name("TOKEN_FILE")
                        .required(true),
                )
                .arg(
                    Arg::with_name("sequence")
                        .short("s")
                        .help("Output sequence file")
                        .value_name("SEQUENCE_FILE")
                        .required(true),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("lex") {
        lex_command(matches)
    }
    if let Some(matches) = matches.subcommand_matches("parse") {
        parse_command(matches)
    }
}
