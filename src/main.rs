extern crate hack_assembler;
extern crate clap;

use clap::{App, Arg,};
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

macro_rules! error {
  ($($arg:tt)*) => ({
      println!($($arg)*);
      return;
  })
}

fn main() {
  const INPUT_EXTENSION: &'static str = "asm";
  const OUTPUT_EXTENSION: &'static str = "hack";
  let matches = App::new("Hack Assembler")
    .version("1.0")
    .author("Kevin Li <kevinli020508@gmail.com>")
    .about("Assembler for the Hack computer as a part of the Nand to Tetris course.")
    .arg(
      Arg::with_name("input")
        .short("i")
        .help(&format!(
          "Sets the input Hack symbolic program to assemble, file extension should be `.{}`",
          INPUT_EXTENSION
        ))
        .takes_value(true)
        .required(true),
    )
    .arg(
      Arg::with_name("output")
        .short("o")
        .help(&format!(
          "Sets the output file to hold the assembled binary code, file exntesion should be `.{}`",
          OUTPUT_EXTENSION
        ))
        .takes_value(true),
    )
    .get_matches();
  let input_path = Path::new(matches.value_of("input").unwrap());
  if !input_path.exists() {
    error!(
      "Input file `{}` doesn't exist. Maybe you had a typo?",
      input_path.display()
    );
  }
  if !input_path.is_file() {
    error!("Input path `{}` points to a directory instead of an `.{}` file.\nTry passing in a file path.", input_path.display(), INPUT_EXTENSION);
  }
  let print_extension_error = || {
    error!("Input file `{}` doesn't have a valid extension. Should end with `.{}` for an assembly input.", input_path.file_name().unwrap().to_str().unwrap(), INPUT_EXTENSION);
  };
  match input_path.extension() {
    Some(extension) => {
      if extension != "asm" {
        print_extension_error();
        return;
      }
    }
    None => {
      print_extension_error();
      return;
    }
  }
  let default_output_path_str = &(input_path
    .to_str()
    .unwrap()
    .replace(&format!(".{}", INPUT_EXTENSION), &format!(".{}", OUTPUT_EXTENSION)));
  let default_output_path = Path::new(default_output_path_str);
  let output_path = matches
    .value_of("output")
    .map_or(default_output_path, |path_str| Path::new(path_str));
  let print_extension_error = || {
    error!("Output file `{}` doesn't have a valid extension. Should end with `.{}` for an binary output.", output_path.file_name().unwrap().to_str().unwrap(), OUTPUT_EXTENSION)
  };
  match output_path.extension() {
    Some(extension) => {
      if extension != OUTPUT_EXTENSION {
        print_extension_error();
        return;
      }
    }
    None => {
      print_extension_error();
      return;
    }
  }

  let mut input_file = match File::open(&input_path) {
    Err(why) => error!("I couldn't open {}: {}.", input_path.display(), why),
    Ok(file) => file,
  };

  // Read the file contents into a string, returns `io::Result<usize>`
  let mut input_str = String::new();
  match input_file.read_to_string(&mut input_str) {
    Err(why) => error!("I couldn't read {}: {}.", input_path.display(), why),
    Ok(_) => println!("Loaded input file {}.", input_path.display()),
  }

  let output = match hack_assembler::assemble(&input_str) {
    Ok(output) => {
      println!(
        "Assembled program {}",
        input_path.file_name().unwrap().to_str().unwrap()
      );
      output
    }
    Err(error) => {
      error!("{}", error);
    }
  };

  let mut output_file = match File::create(&output_path) {
    Err(why) => error!("I couldn't create {}: {}.", output_path.display(), why),
    Ok(file) => file,
  };

  match output_file.write_all(output.as_bytes()) {
    Err(why) => error!("I couldn't write to {}: {}.", output_path.display(), why),
    Ok(_) => println!("Wrote to {}.", output_path.display()),
  }
}
