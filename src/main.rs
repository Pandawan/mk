mod ast;
mod lexer;
mod parser;
mod position;
mod repl;
mod token;

fn main() {
    repl::repl();
}
