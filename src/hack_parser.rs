#[path = "utils/parser.rs"]
mod parser;

use lazy_static::lazy_static;
use parser::*;
use std::collections::HashSet;

lazy_static! {
  static ref COMPUTATION_INSTRUCTIONS: HashSet<&'static str> = vec![
    "0",
    "1",
    "-1",
    "D",
    "A",
    "!D",
    "!A",
    "-D",
    "-A",
    "D+1",
    "A+1",
    "D-1",
    "A-1",
    "D+A",
    "D-A",
    "A-D",
    "D&A",
    "D|A",
    "M",
    "!M",
    "-M",
    "M+1",
    "M-1",
    "D+M",
    "D-M",
    "M-D",
    "D&M",
    "D|M",
  ]
  .into_iter()
  .collect();
}
#[derive(Debug)]
pub enum Instruction {
  A(usize),
  C {
    destinations: Option<Destinations>,
    computation: String,
    jump: Option<Jump>,
  },
  Ignored,
}

#[derive(Copy, Clone, Debug)]
pub struct Destinations {
  A: bool,
  D: bool,
  M: bool,
}

#[derive(Clone, Debug)]
pub struct Jump {
  LT: bool,
  EQ: bool,
  GT: bool,
}

pub fn parse<'a>(source: &'a str) -> Result<Vec<Instruction>, String> {
  let output = one_or_more_till_end(move |input, location| match token("@").parse(input, location) {
    ParseResult::ParseOk { .. } => a_instruction().parse(input, location),
    ParseResult::ParseError { .. } => either(ignored(), c_instruction()).parse(input, location),
  })
  .parse(source, Location { row: 1, col: 1 });
  match output {
    ParseResult::ParseOk { output, .. } => Ok(output),
    ParseResult::ParseError {
      message: error_message,
      from,
      to,
    } => Err(display_error(source, error_message, from, to)),
  }
}

// @3
fn a_instruction<'a>() -> BoxedParser<'a, Instruction> {
  right(
    token("@"),
    left(whole_decimal().pred(|number| *number <= 32767, "a decimal number <= 32767 (2^15 - 1)"), newline_with_comment("//")),
  )
  .map(|number| Instruction::A(number))
}

// D=D+A;JMP
fn c_instruction<'a>() -> BoxedParser<'a, Instruction> {
  optional(
    None,
    left(
      destinations().map(|destinations| Some(destinations)),
      token("="),
    ),
  )
  .and_then(|destinations| {
    one_or_more(any_char().pred(
      |character| *character != ';' && *character != '\n',
      "a computation instruction",
    ))
    .and_then(|characters| {
      let comp_instruction = characters.iter().collect::<String>();
      move |input, location|
        match COMPUTATION_INSTRUCTIONS.get::<str>(&comp_instruction) {
          Some(instruction) =>
            ParseResult::ParseOk {
              input,
              location,
              output: instruction,
            },
          None =>
            ParseResult::ParseError {
              message: format!("I can't find a computation instruction matching `{}`.\nTry something like `D+1` and `0`.", comp_instruction),
              from: Location {
                row: location.row,
                col: location.col - comp_instruction.len(),
              },
              to: location,
            }
        }
    }
    )
    .and_then(move |computation| {
      left(
        optional(
          None,
          right(
            token(";"),
            choose3(
              choose3(
                token("JGT").map(|_| Jump {
                  LT: false,
                  EQ: false,
                  GT: true,
                }),
                token("JEQ").map(|_| Jump {
                  LT: false,
                  EQ: true,
                  GT: false,
                }),
                token("JGE").map(|_| Jump {
                  LT: false,
                  EQ: true,
                  GT: true,
                }),
              ),
              choose3(
                token("JLT").map(|_| Jump {
                  LT: true,
                  EQ: false,
                  GT: false,
                }),
                token("JNE").map(|_| Jump {
                  LT: true,
                  EQ: false,
                  GT: true,
                }),
                token("JLE").map(|_| Jump {
                  LT: true,
                  EQ: true,
                  GT: false,
                }),
              ),
              token("JMP").map(|_| Jump {
                LT: true,
                EQ: true,
                GT: true,
              }),
            ),
          )
          .map(|jump| Some(jump)),
        )
        .map(move |jump| Instruction::C {
          destinations,
          computation: computation.to_string(),
          jump,
        }),
        newline_with_comment("//"),
      )
    })
  })
}

fn destinations<'a>() -> impl Parser<'a, Destinations> {
  choose3(
    choose3(
      token("AMD").map(|_| Destinations {
        A: true,
        D: true,
        M: true,
      }),
      token("AD").map(|_| Destinations {
        A: true,
        D: true,
        M: false,
      }),
      token("AM").map(|_| Destinations {
        A: true,
        D: false,
        M: true,
      }),
    ),
    choose3(
      token("MD").map(|_| Destinations {
        A: false,
        D: true,
        M: true,
      }),
      token("M").map(|_| Destinations {
        A: false,
        D: false,
        M: true,
      }),
      token("D").map(|_| Destinations {
        A: false,
        D: true,
        M: false,
      }),
    ),
    token("A").map(|_| Destinations {
      A: true,
      D: false,
      M: false,
    }),
  )
}

fn ignored<'a>() -> BoxedParser<'a, Instruction> {
  either(line_comment("//"), newline_char()).map(|_| Instruction::Ignored)
}
