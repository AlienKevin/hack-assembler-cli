mod hack_assembler;
mod hack_emitter;
mod hack_parser;

extern crate clap;
use clap::{Arg, App, SubCommand};
use core::fmt::Debug;
use core::fmt::Display;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
  const INPUT_EXTENSION: &'static str = "asm";
  const OUTPUT_EXTENSION: &'static str = "hack";
  let matches = App::new("Hack Assembler")
  .version("1.0")
  .author("Kevin Li <kevinli020508@gmail.com>")
  .about("Assembler for the Hack computer as a part of the Nand to Tetris course.")
  .arg(Arg::with_name("input")
    .short("i")
    .help(&format!("Sets the input Hack symbolic program to assemble, file extension should be `.{}`", INPUT_EXTENSION))
    .takes_value(true)
    .required(true))
  .arg(Arg::with_name("output")
      .short("o")
      .help(&format!("Sets the output file to hold the assembled binary code, file exntesion should be `.{}`", OUTPUT_EXTENSION))
      .takes_value(true)
    )
  .get_matches();
  let input_path = Path::new(matches.value_of("input").unwrap());
  if !input_path.exists() {
    println!("Input file `{}` doesn't exist. Maybe you had a typo?", input_path.display());
    return;
  }
  if !input_path.is_file() {
    println!("Input path `{}` points to a directory instead of an `.{}` file.\nTry passing in a file path.", input_path.display(), INPUT_EXTENSION);
    return;
  }
  let print_extension_error = || println!("Input file `{:#?}` doesn't have a valid extension. Should end with `.{}` for an assembly input.", input_path.file_name().unwrap(), INPUT_EXTENSION);
  match input_path.extension() {
    Some(extension) =>
      if extension != "asm" {
        print_extension_error()
      },
    None =>
      print_extension_error()
  }
  let output_path = matches.value_of("output").map_or(input_path, |path_str| Path::new(path_str));
  let print_extension_error = || println!("Output file `{}` doesn't have a valid extension. Should end with `.hack` for an binary output.", output_path.file_name().unwrap().to_str().unwrap());
  match output_path.extension() {
    Some(extension) =>
      if extension != OUTPUT_EXTENSION {
        print_extension_error()
      },
    None =>
      print_extension_error()
  }

  let mut input_file = match File::open(&input_path) {
    Err(why) => panic!("I couldn't open {}: {}.", input_path.display(), why),
    Ok(file) => file,
  };

  // Read the file contents into a string, returns `io::Result<usize>`
  let mut input_str = String::new();
  match input_file.read_to_string(&mut input_str) {
    Err(why) => panic!("I couldn't read {}: {}.", input_path.display(), why),
    Ok(_) => println!("Loaded input file {}.", input_path.display()),
  }

  let output = match hack_assembler::assemble(&input_str) {
    Ok(output) => {
      println!("Assembled program {}", input_path.file_name().unwrap().to_str().unwrap());
      output
    },
    Err(error) => {
      println!("{}", error);
      return;
    }
  };

  let mut output_file = match File::create(&output_path) {
      Err(why) => panic!("I couldn't create {}: {}.", output_path.display(), why),
      Ok(file) => file,
  };

  match output_file.write_all(output.as_bytes()) {
      Err(why) => panic!("I couldn't write to {}: {}.", output_path.display(), why),
      Ok(_) => println!("Wrote to {}.", output_path.display()),
  }
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
