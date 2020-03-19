#[path = "utils/parser.rs"]
mod parser;

use parser::*;

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
  let output = one_or_more_till_end(
    move |input, state|
    match token("@").parse(input, state) {
      ParseResult::ParseOk { .. } =>
        a_instruction().parse(input, state),
      ParseResult::ParseError { .. } =>
        either(
          ignored(),
          c_instruction(),
        ).parse(input, state)
    }
  )
    .parse(source, ParseState { row: 1, col: 1 });
  match output {
    ParseResult::ParseOk { output, .. } => Ok(output),
    ParseResult::ParseError {
      message: error_message,
      state: error_state,
    } => Err(display_error(source, error_message, error_state)),
  }
}

// @3
fn a_instruction<'a>() -> BoxedParser<'a, Instruction> {
  right(
    token("@"),
    left(whole_decimal(), newline_with_comment("//")),
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
    .map(|characters| characters.iter().collect::<String>())
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
          computation: computation.clone(),
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
