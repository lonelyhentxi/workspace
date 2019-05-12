extern crate docopt;
#[macro_use]
extern crate serde_derive;
extern crate rustc_serialize;
extern crate serde;
extern crate yatc;

use yatc::driver::{main_loop, Tokens, AST, Jit,Interpreter, Target};

use docopt::Docopt;

const USAGE: &str = "
Usage: yatc [(-l | -p | -j | -i | -t)]
Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
    -j  Run only jit executor and show its output.
    -i  Run only interpreter executor and show its output.
    -t  Run only compiler and show its output.
";

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize, RustcDecodable)]
struct Args {
    flag_l: bool,
    flag_p: bool,
    flag_i: bool,
    flag_j: bool,
    flag_t: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let stage = if args.flag_l {
        Tokens
    } else if args.flag_p {
        AST
    } else if args.flag_i {
        Interpreter
    } else if args.flag_j {
        Jit
    } else {
        Target
    };
    main_loop(stage);
}
