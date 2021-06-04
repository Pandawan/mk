mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    // repl::repl();

    let input = "
    let test = 10;
    ";
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    match parser.parse_program() {
        Ok(program) => {
            for statement in program.statements {
                println!("{:?}", statement)
            }
        }
        Err(errors) => {
            for error in errors {
                println!("Error: {}", error)
            }
        }
    }
}
