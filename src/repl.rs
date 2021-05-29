use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::lexer::Lexer;
use crate::token::Token;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub fn repl() {
    println!("mk language v{}", VERSION);

    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line == "exit" || line == "quit" {
                    break;
                }

                rl.add_history_entry(line.as_str());

                let mut l = Lexer::new(&line);

                loop {
                    match l.next_token() {
                        Token::Eof => break,
                        tok => println!("{:?}", tok),
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
