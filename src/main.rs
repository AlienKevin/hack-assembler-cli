mod hack_parser;
mod hack_emitter;

use core::fmt::Debug;
use core::fmt::Display;

fn main() {
    let source =
"// This file is part of www.nand2tetris.org
// and the book \"The Elements of Computing Systems\"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@32767
D=A
@3
D=D+A
@0
M=D
";
    print_result(assemble(source));
}

fn assemble(source: &str) -> Result<String, String>
{
  hack_parser::parse(source).map(
      |ast| hack_emitter::emit(ast)
  )
}

fn print_result<T, U>(result: Result<T, U>) -> ()
  where
    T: Debug,
    U: Display,
{
  match result {
    Ok(output) => println!("{:#?}", output),
    Err(error) => println!("{}", error),
  }
}