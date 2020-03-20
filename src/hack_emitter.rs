use crate::hack_parser::*;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
  static ref COMPUTATION_INSTRUCTIONS: HashMap<&'static str, &'static str> = [
    ("0", "1101010"),
    ("1", "1111111"),
    ("-1", "1111010"),
    ("D", "1001100"),
    ("A", "1110000"),
    ("!D", "1001101"),
    ("!A", "1110001"),
    ("-D", "1001111"),
    ("-A", "1110011"),
    ("D+1", "1011111"),
    ("A+1", "1110111"),
    ("D-1", "1001110"),
    ("A-1", "1110010"),
    ("D+A", "1000010"),
    ("D-A", "1010011"),
    ("A-D", "1000111"),
    ("D&A", "1000000"),
    ("D|A", "1010101"),
    ("M", "0110000"),
    ("!M", "0110001"),
    ("-M", "0110011"),
    ("M+1", "0110111"),
    ("M-1", "0110010"),
    ("D+M", "0000010"),
    ("D-M", "0010011"),
    ("M-D", "0000111"),
    ("D&M", "0000000"),
    ("D|M", "0010101"),
  ]
  .iter()
  .cloned()
  .collect();
}

pub fn emit(instructions: Vec<Instruction>) -> String {
  instructions
    .iter()
    .map(|instruction| match instruction {
      Instruction::A(number) => format!("{:015b}\n", number),
      Instruction::C {
        destinations,
        computation,
        jump,
      } => format!(
        "111{}{}{}\n",
        emit_destinations(destinations),
        emit_computation(computation),
        emit_jump(jump)
      ),
      Instruction::Ignored => "".to_string(),
    })
    .collect::<Vec<String>>()
    .join("")
}

fn emit_destinations(destinations: &Option<Destinations>) -> String {
  match destinations {
    Some(dest) => format!(
      "{}{}{}",
      bool_to_bit(dest.A),
      bool_to_bit(dest.D),
      bool_to_bit(dest.M)
    ),
    None => "".to_string(),
  }
}

fn emit_computation(computation: &String) -> String {
  COMPUTATION_INSTRUCTIONS
    .get::<str>(&computation)
    .unwrap()
    .to_string()
}

fn emit_jump(jump: &Option<Jump>) -> String {
  match jump {
    Some(j) => format!(
      "{}{}{}",
      bool_to_bit(j.LT),
      bool_to_bit(j.EQ),
      bool_to_bit(j.GT)
    ),
    None => "".to_string()
  }
}

fn bool_to_bit(boolean: bool) -> String {
  (if boolean { "1" } else { "0" }).to_string()
}
