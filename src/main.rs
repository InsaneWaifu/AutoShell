use std::num::NonZeroU64;
use yash_syntax::{parser::lex::Lexer, alias::AliasSet, parser::Parser, source::Source};
mod cmd;
fn main() {
    let cmd = "echo \"Hello World $PATH $(ls -la)\" > file.txt";
    let mem = yash_syntax::input::Memory::new(cmd);
  let input = Box::new(mem);
  // Next, create a lexer.
  let line = NonZeroU64::new(1).unwrap();
  let mut lexer = Lexer::new(input, line, Source::Unknown);

  // Then, create a new parser borrowing the lexer.
  let aliases = AliasSet::new();
  let mut parser = Parser::new(&mut lexer, &aliases);

  // Lastly, call the parser's function to get an AST.
  use futures_executor::block_on;
  let list = block_on(parser.command_line()).unwrap().unwrap();
  dbg!(list);
}
