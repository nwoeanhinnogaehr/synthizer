#![feature(plugin, optin_builtin_traits)]
#![plugin(regex_macros, docopt_macros)]

extern crate docopt;
extern crate rustc_serialize;
extern crate vec_map;
#[macro_use]
extern crate interpreter;


docopt!(Args, "
Usage:
  synthizer stream <input>
  synthizer write <input> <output> [--length=<sec>]
  synthizer --help

Options:
  -h, --help             Show this message.
  -l, --length=<sec>     Length of audio to render, in seconds [default: 32].
", flag_length: f32);

use interpreter::parse::synthizer;
use interpreter::util::read_file;

#[allow(dead_code)]
fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    let filename = args.arg_input;
    let source = read_file(&filename).unwrap();
    println!("{:?}", synthizer::parse_Grammar(&source));
}
