use std::cell::RefCell;
use std::rc::Rc;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn repl() {
    println!("mk language v{}", VERSION);

    let env = Rc::new(RefCell::new(Environment::new()));

    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "exit" || line == "quit" {
                    break;
                }
                // Skip empty lines
                else if line.trim().is_empty() {
                    continue;
                }

                rl.add_history_entry(line.as_str());

                let l = Lexer::new(&line);
                let mut p = Parser::new(l);
                let prog = p.parse_program();

                match prog {
                    Ok(prog) => {
                        let mut e = Evaluator::new_with_env(Rc::clone(&env));
                        let value = e.eval(prog);

                        println!("{}", value);
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
