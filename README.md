# Synthizer
A simple experimental language for real time additive audio synthesis, intended for the creation of unique mathematical sounds.

## What works so far
- Lexing
- Parsing
- Typechecking
- Codegen (LLVM backend)
- Audio output (WAV)
- Real time audio output

## Todo
- Documentation
- GPU backend
- GUI
- MIDI (maybe) or at least some interface for a DAW
- Graphical representations of the sound

## Example code
Check the examples directory for example synths.
You can listen to them in real time with:
```
cargo run -- stream examples/???.synt
```
