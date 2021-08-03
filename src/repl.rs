use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;

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

                let l = Lexer::new(&line);
                let mut p = Parser::new(l);
                let prog = p.parse_program();

                match prog {
                    Ok(prog) => {
                        let e = Evaluator::new();

                        if let Some(obj) = e.eval(prog) {
                            println!("{}", obj);
                        }
                    }
                    Err(errors) => {
                        println!("Parser errors:");
                        for error in errors {
                            println!("\t{}", error);
                        }
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
