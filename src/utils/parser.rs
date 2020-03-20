use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Located<A> {
  pub value: A,
  pub from: Location,
  pub to: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
  pub row: usize,
  pub col: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseResult<'a, Output> {
  ParseOk {
    input: &'a str,
    location: Location,
    output: Output,
  },
  ParseError {
    message: String,
    from: Location,
    to: Location,
  },
}

impl<'a, T> ParseResult<'a, T> {
  pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> ParseResult<'a, U> {
    match self {
      ParseResult::ParseOk { input, location, output } =>
        ParseResult::ParseOk {
          input,
          location,
          output: func(output)
        },
      ParseResult::ParseError { message, from, to } =>
        ParseResult::ParseError { message, from, to },
    }
  }
  pub fn map_err<F: FnOnce(String) -> String>(self, func: F) -> ParseResult<'a, T> {
    match self {
      ParseResult::ParseOk { input, location, output } =>
        ParseResult::ParseOk {
          input,
          location,
          output,
        },
      ParseResult::ParseError { message, from, to } =>
        ParseResult::ParseError {
          message: func(message),
          from,
          to,
        }
    }
  }
  pub fn and_then<U, F: FnOnce(&'a str, T, Location) -> ParseResult<'a, U>>(self, func: F) -> ParseResult<'a, U> {
    match self {
      ParseResult::ParseOk { input, output, location } =>
        func(input, output, location),
      ParseResult::ParseError { message, from, to } =>
        ParseResult::ParseError { message, from, to },
    }
  }
}

pub trait Parser<'a, Output> {
  fn parse(&self, input: &'a str, location: Location) -> ParseResult<'a, Output>;
  fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
  {
      BoxedParser::new(map(self, map_fn))
  }
  fn map_err<F>(self, map_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(String) -> String + 'a,
  {
      BoxedParser::new(map_err(self, map_fn))
  }
  fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
  where
    Self: Sized + 'a,
    Output: 'a,
    NewOutput: 'a,
    NextParser: Parser<'a, NewOutput> + 'a,
    F: Fn(Output) -> NextParser + 'a,
  {
      BoxedParser::new(and_then(self, f))
  }
  fn pred<F>(self, predicate: F, expecting: &'a str) -> BoxedParser<'a, Output>
  where
    Self: Sized + 'a,
    Output: std::fmt::Display + 'a,
    F: Fn(&Output) -> bool + 'a,
  {
    BoxedParser::new(pred(self, predicate, expecting))
  }
  fn ignore(self) -> BoxedParser<'a, ()>
    where
      Self: Sized + 'a,
      Output: 'a,
  {
      BoxedParser::new(map(self, |_| ()))
  }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
  F: Fn(&'a str, Location) -> ParseResult<Output>,
{
  fn parse(&self, input: &'a str, location: Location) -> ParseResult<'a, Output> {
    self(input, location)
  }
}

pub struct BoxedParser<'a, Output> {
  parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
  pub fn new<P>(parser: P) -> Self
  where
      P: Parser<'a, Output> + 'a,
  {
      BoxedParser {
          parser: Box::new(parser),
      }
  }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
  fn parse(&self, input: &'a str, location: Location) -> ParseResult<'a, Output> {
      self.parser.parse(input, location)
  }
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
  where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP
{
  move |input, location| parser.parse(input, location)
    .and_then(| next_input, next_output, next_location |
      f(next_output).parse(next_input, next_location)
    )
}

pub fn token<'a>(expected: &'static str) -> BoxedParser<'a, &str> {
  BoxedParser::new(
    move |input: &'a str, location: Location| {
    let found = input.get(0..expected.len());
    match found {
      Some(next) if next == expected => ParseResult::ParseOk {
        input: &input[expected.len()..],
        output: expected,
        location: increment_col(expected.len(), location),
      },
      _ => ParseResult::ParseError {
        message: format!(
          "I'm expecting a `{}` but found {}.",
          expected,
          display_token(found)
        ),
        from: location,
        to: match found {
          Some(found_str) => Location {
            row: location.row + found_str.len(),
            col: location.col,
          },
          None => location,
        }
      },
    }
  })
}

pub fn increment_col(additional_col: usize, location: Location) -> Location {
  Location {
    col: location.col + additional_col,
    ..location
  }
}

pub fn increment_row(additional_row: usize, location: Location) -> Location {
  Location {
    row: location.row + additional_row,
    col: 1,
  }
}

pub fn display_token<T: fmt::Display>(token: Option<T>) -> String {
  match token {
    Some(token_content) => format!("`{}`", token_content).replace("\n", "\\n"),
    None => "nothing".to_string(),
  }
}

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
  P1: Parser<'a, R1>,
  P2: Parser<'a, R2>,
{
  move |input, location|
    parser1.parse(input, location)
      .and_then(| next_input, first_output, next_location |
        parser2.parse(next_input, next_location).map(| second_output |
          (first_output, second_output)
        )
      )
}

pub fn quadruple<'a, P1: 'a, P2: 'a, P3: 'a, P4: 'a, R1: 'a, R2: 'a, R3: 'a, R4: 'a>
  (parser1: P1, parser2: P2, parser3: P3, parser4: P4)
  -> BoxedParser<'a, (R1, R2, R3, R4)>
  where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    P3: Parser<'a, R3>,
    P4: Parser<'a, R4>,
{
  pair(
    pair(parser1, parser2),
    pair(parser3, parser4),
  )
  .map(|((result1, result2), (result3, result4))|
    (result1, result2, result3, result4)
  )
}

pub fn map<'a, P: 'a, F: 'a, A, B>(parser: P, map_fn: F) -> BoxedParser<'a, B>
where
  P: Parser<'a, A>,
  F: Fn(A) -> B,
{
  BoxedParser::new(
    move |input, location| parser.parse(input, location).map(
    |output| map_fn(output)
  ))
}

pub fn map_err<'a, P, F, A>(parser: P, map_fn: F) -> impl Parser<'a, A>
where
  P: Parser<'a, A>,
  F: Fn(String) -> String,
{
  move |input, location| parser.parse(input, location).map_err(
    |error_message| map_fn(error_message)
  )
}

fn map2<'a, P1, P2, F, A, B, C>(parser1: P1, parser2: P2, map_fn: F) -> impl Parser<'a, C>
where
  P1: Parser<'a, A>,
  P2: Parser<'a, B>,
  F: Fn(A, B) -> C,
{
  move |input, location| parser1.parse(input, location).and_then(
    |input1, output1, state1 | parser2.parse(input1, state1).map(
      |output2| map_fn(output1, output2)
    )
  )
}

pub fn left<'a, P1: 'a, P2: 'a, R1: 'a, R2: 'a>(parser1: P1, parser2: P2) -> BoxedParser<'a, R1>
where
  P1: Parser<'a, R1>,
  P2: Parser<'a, R2>,
{
  map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1: 'a, P2: 'a, R1: 'a, R2: 'a>(parser1: P1, parser2: P2) -> BoxedParser<'a, R2>
where
  P1: Parser<'a, R1>,
  P2: Parser<'a, R2>,
{
  map(pair(parser1, parser2), |(_left, right)| right)
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  one_or_more_with_ending(false, parser)
}

pub fn one_or_more_till_end<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  one_or_more_with_ending(true, parser)
} 

pub fn one_or_more_with_ending<'a, P, A>(till_end: bool, parser: P) -> impl Parser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  move |mut input, mut location| {
    let mut result = Vec::new();

    match parser.parse(input, location) {
      ParseResult::ParseOk {
        input: next_input,
        output: first_item,
        location: next_location,
      } => {
        input = next_input;
        location = next_location;
        result.push(first_item);
      }
      ParseResult::ParseError {
        message: error_message,
        from,
        to,
      } => {
        return ParseResult::ParseError {
          message: error_message,
          from,
          to,
        };
      }
    }

    loop {
      match parser.parse(input, location) {
        ParseResult::ParseOk {
          input: next_input,
          output: next_item,
          location: next_location,
        } => {
          input = next_input;
          location = next_location;
          result.push(next_item);
        },
        ParseResult::ParseError {
          message: error_message,
          from,
          to,
        } => if till_end && input != "" {
          return ParseResult::ParseError {
            message: error_message,
            from,
            to,
          };
        } else {
          break;
        }
      }
    }

    ParseResult::ParseOk {
      input: input,
      output: result,
      location: location,
    }
  }
}

pub fn zero_or_more<'a, P: 'a, A>(parser: P) -> BoxedParser<'a, Vec<A>>
where
  P: Parser<'a, A>,
{
  BoxedParser::new(
    move |mut input, mut location| {
    let mut result = Vec::new();

    while let ParseResult::ParseOk {
      input: next_input,
      output: next_item,
      location: next_location,
    } = parser.parse(input, location)
    {
      input = next_input;
      location = next_location;
      result.push(next_item);
    }

    ParseResult::ParseOk {
      input: input,
      output: result,
      location: location,
    }
  })
}

pub fn any_char<'a>() -> impl Parser<'a, char> {
  |input: &'a str, location: Location| match input.chars().next() {
    Some(character) => ParseResult::ParseOk {
      input: &input[character.len_utf8()..],
      output: character,
      location: increment_col(character.len_utf8(), location),
    },
    _ => ParseResult::ParseError {
      message: "I'm expecting any character but reached the end of input.".to_string(),
      from: location,
      to: location,
    },
  }
}

fn pred<'a, P, F, A: std::fmt::Display>(parser: P, predicate: F, expecting: &'a str) -> impl Parser<'a, A>
where
  P: Parser<'a, A>,
  F: Fn(&A) -> bool,
{
  move |input, location| match parser.parse(input, location) {
    ParseResult::ParseOk {
      input: next_input,
      output: content,
      location: next_location,
    } => if predicate(&content) {
      ParseResult::ParseOk {
        input: next_input,
        output: content,
        location: next_location,
      }
    } else {
      ParseResult::ParseError {
        message: format!(
          "I'm expecting {} but found {}.",
          expecting,
          display_token(Some(content)),
        )
        .to_string(),
        from: location,
        to: next_location,
        }
    },
    _ => ParseResult::ParseError {
      message: format!(
        "I'm expecting {} but found {}.",
        expecting,
        display_token(input.chars().next())
      )
      .to_string(),
      from: location,
      to: location,
    },
  }
}

pub fn space_char<'a>() -> BoxedParser<'a, ()> {
  any_char().pred(
    |character| *character == ' ',
    "a whitespace",
  ).ignore()
}

pub fn newline_char<'a>() -> BoxedParser<'a, ()> {
  BoxedParser::new(
    (move |input, location| {
      let mut next_input: &str = input;
      let mut next_location: Location = location;
      let result1 = any_char().pred(
        |character| *character == '\r',
        "a carriage return",
      ).parse(input, location);
      match result1 {
        ParseResult::ParseOk {
          input,
          location,
          ..
        } => {
          next_input = input;
          next_location = location;
        }
        _ => {}
      }
      let result = any_char().pred(
        |character| *character == '\n',
        "a newline",
      ).parse(next_input, next_location);
      match result {
        ParseResult::ParseOk {
          input: next_input,
          output,
          location: next_location,
        } => ParseResult::ParseOk {
          input: next_input,
          output: output,
          location: increment_row(1, next_location)
        },
        ParseResult::ParseError {
          message: error_message,
          from,
          to,
        } => ParseResult::ParseError {
          message: error_message,
          from,
          to,
        }
      }
    }).ignore()
  )
}

fn newline0<'a>(indentations: usize) -> BoxedParser<'a, ()> {
  zero_or_more(
    ignore_chain(vec![
      indents(indentations),
      newline_char(),
    ])
  ).ignore()
}

pub fn newline1<'a>(indentations: usize) -> BoxedParser<'a, ()> {
  ignore_chain(vec![
    newline_char(),
    newline0(indentations),
  ])
}

pub fn space0<'a>() -> BoxedParser<'a, ()> {
  zero_or_more(space_char()).ignore()
}

pub fn space1<'a>() -> BoxedParser<'a, ()> {
  one_or_more(space_char()).ignore()
}

pub fn indent<'a>() -> BoxedParser<'a, ()> {
  ignore_chain(vec![
    space_char(),
    space_char(),
  ]).map_err(|_| "I'm expecting an indentation.\nAll indentations should be two spaces.".to_string())
}

pub fn indents<'a>(indentations: usize) -> BoxedParser<'a, ()> {
  repeat(
    indentations,
    indent(),
  ).map_err(|_| "I'm expecting an indentation.\nAll indentations should be two spaces.".to_string())
  .ignore()
}

fn repeat<'a, A, P>(times: usize, parser: P)
  -> impl Parser<'a, Vec<A>>
  where
    P: Parser<'a, A>
{
  move |mut input, mut location| {
    let mut result = Vec::new();

    if times == 0 {
      return ParseResult::ParseOk {
        input,
        location,
        output: result,
      }
    }

    let mut counter = 0;

    while let ParseResult::ParseOk {
      input: next_input,
      output: next_item,
      location: next_location,
    } = parser.parse(input, location)
    {
      if counter >= times {
        break;
      }
      input = next_input;
      location = next_location;
      result.push(next_item);
      counter = counter + 1;
    }

    ParseResult::ParseOk {
      input: input,
      output: result,
      location: location,
    }
  }
}

pub fn choose3<'a, A: 'a, P: 'a>(parser1: P, parser2: P, parser3: P)
  -> BoxedParser<'a, A>
  where
    P: Parser<'a, A>
{
  either(
    BoxedParser::new(parser1),
    either(
      parser2,
      parser3,
    )
  )
}

pub fn either<'a, A, P: 'a>(parser1: P, parser2: P)
  -> BoxedParser<'a, A>
  where
    P: Parser<'a, A>
{
  BoxedParser::new(
    move |input, location|
      match parser1.parse(input, location) {
        ok @ ParseResult::ParseOk {..} => ok,
        ParseResult::ParseError {..} =>
          parser2.parse(input, location)
      }
  )
}

pub fn optional<'a, A: Clone + 'a, P: 'a>(default: A, parser: P)
  -> BoxedParser<'a, A>
  where
    P: Parser<'a, A>
{
  either(
    BoxedParser::new(
      parser
    ),
    BoxedParser::new(
      move |input, location|
      ParseResult::ParseOk {
          input,
          location,
          output: default.clone(),
        }
      )
  )
}

pub fn newline_with_comment<'a>(comment_symbol: &'static str) -> impl Parser<'a, ()> {
  either(
    ignore_chain(vec![
      space0(),
      line_comment(comment_symbol),
    ]),
    newline_char(),
  )
}

pub fn line_comment<'a>(comment_symbol: &'static str) -> BoxedParser<'a, ()> {
  ignore_chain(vec![
    token(comment_symbol).ignore(),
    zero_or_more(any_char().pred(
      |character| *character != '\n' && *character != '\r',
      "any character",
    )).ignore(),
    newline_char(),
  ])
}

pub fn line_comments<'a>(indentations: usize) -> BoxedParser<'a, ()> {
  either(
    one_or_more(
      ignore_chain(vec![
        newline0(indentations),
        indents(indentations),
        token("--").ignore(),
        zero_or_more(any_char().pred(
          |character| *character != '\n' && *character != '\r',
          "any character",
        )).ignore(),
        newline1(indentations),
      ])
    ).ignore(),
    newline0(indentations),
  )
}

pub fn ignore_chain<'a>(parsers: Vec<BoxedParser<'a, ()>>) -> BoxedParser<'a, ()>
{
  BoxedParser::new(
    move | mut input, mut location | {
    for parser in &parsers {
      match parser.parse(input, location) {
        ParseResult::ParseOk {
          input: next_input,
          location: next_location,
          ..
        } => {
          input = next_input;
          location = next_location;
        },
        error @ ParseResult::ParseError {..} => {
          return error;
        }
      }
    }
    ParseResult::ParseOk {
      input,
      location,
      output: (),
    }
  })
}

pub fn whole_decimal<'a>() -> impl Parser<'a, usize> {
  one_or_more(
    any_char().pred(
    | character |
      character.is_digit(10)
    , "a whole decimal number"
    )
  ).map(| digits | digits.iter().collect::<String>().parse().unwrap())
}

pub fn located<'a, P: 'a, A>(parser: P) -> impl Parser<'a, Located<A>>
  where
    P: Parser<'a, A>
{
  move |input, location|
  match parser.parse(input, location) {
    ParseResult::ParseOk {
      input: next_input,
      output,
      location: next_location
    } => ParseResult::ParseOk {
        input: next_input,
        output: Located {
          value: output,
          from: Location {
            row: location.row,
            col: location.col,
          },
          to: Location {
            row: next_location.row,
            col: next_location.col,
          },
        },
        location: next_location,
      },
    ParseResult::ParseError {
      message: error_message,
      from,
      to,
    } =>
      ParseResult::ParseError {
        message: error_message,
        from,
        to,
      }
  }
}

pub fn display_error(source: &str, error_message: String, from: Location, to: Location) -> String {
  let row = from.row;
  let col = from.col;
  let error_length = if to.col == from.col {
    1
  } else {
    to.col - from.col
  };
  let error_line = row.to_string() + "| " + source.split("\n").collect::<Vec<&str>>()[row - 1];
  let error_pointer = " ".repeat(col - 1 + row.to_string().len() + 2) + &"^".repeat(error_length);
  let error_report =
    error_line + "\n" + &error_pointer + "\n" + "⚠️" + &error_message;
  error_report
}