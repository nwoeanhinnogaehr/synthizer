#![feature(plugin, collections, into_cow, debug_builders)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate rustc_serialize;
extern crate docopt;

pub mod interpreter;

use interpreter::synth::Synth;

docopt!(Args, "
Usage:
  synthizer [--tokens] [--idmap] <file>
  synthizer --help

Options:
  -h, --help       Show this message.
");

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    let filename = args.arg_file;

    let mut synth = Synth::new(&filename).unwrap();
    if synth.compile() {
        println!("OK!");
    }
}
