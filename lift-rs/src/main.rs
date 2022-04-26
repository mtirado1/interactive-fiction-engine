use lift::*;
use std::fs;
use std::process;
use std::env;
use std::io;
use std::io::Write;

enum UserActions {
    Tap(usize),
    Prompt(usize)
}

fn prompt(interpreter: &mut Interpreter, choices: Vec<UserActions>) {
    print!(" -> ");
    let _ = io::stdout().flush();
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read input");
    match input.trim().to_lowercase().as_str() {
        "quit" | "exit" | "q" => process::exit(0),
        "help" | "h" | "?" => print_help(),
        "load" => load_state(interpreter),
        "save" | "s" => save_state(interpreter),
        _ => {
            if let Ok(choice) = input.trim().parse::<u32>() {
                handle_choice(interpreter, choices, choice)
            } else {
                println!("Invalid command.");
            }
        }
    }
}

fn load_state(interpreter: &mut Interpreter) {
    let file = "lift_state.json";
    if let Ok(json) = fs::read_to_string(file) {
        if let Ok(_) = interpreter.load_state(&json) {
            println!("Loading state...");
            return;
        }
    }
    eprintln!("Error while reading file '{}'", file);
}

fn save_state(interpreter: &Interpreter) {
    if let Some(json) = interpreter.dump_state() {
        let path = "lift_state.json";
        if let Ok(mut file) = fs::File::create(path) {
            if let Ok(_) = write!(file, "{}", json) {
                println!("State saved to {}", path);
                return;
            }
        }
    }
    println!("Failed to save state.");
}

fn print_help() {
    println!("lift {} - A Language for Interactive Fiction Texts\n", env!("CARGO_PKG_VERSION"));
    println!(concat!(
        "Command Reference:\n",
        "  quit | exit | q   Quit Lift\n",
        "  save | s          Save state to file\n",
        "  help | h | ?      Display this help message\n",
        "  [N]               Execute action N"
    ));
}

fn handle_choice(interpreter: &mut Interpreter, choices: Vec<UserActions>, choice: u32) {
  if let Some(action) = choices.get((choice - 1) as usize) {
       match action {
           UserActions::Tap(i) => interpreter.send(*i, Value::Null),
           UserActions::Prompt(i) => {
               let mut user_input = String::new();
               print!(" -> ");
               let _ = io::stdout().flush();
               io::stdin()
                   .read_line(&mut user_input)
                   .expect("Failed to read input");
               user_input = user_input.trim().to_string();
               interpreter.send(*i, Value::Text(user_input));
           }
       }
   }
   else {
       println!("Invalid choice.");
   }
}

fn play(interpreter: &mut Interpreter) {
    interpreter.play();
    loop {
        let (content, choices) = render(interpreter);
        println!("--- --- --- ---");
        println!("{}", content);
        println!("--- --- --- ---\n");
        prompt(interpreter, choices)
    }
}

enum LastElement {
    Empty,
    Text,
    Break
}

use LastElement::*;

fn render(interpreter: &Interpreter) -> (String, Vec<UserActions>) {
    let mut ret = String::new();
    let mut input_id = 1;
    let mut choices = Vec::<UserActions>::new();
    let mut last = Empty;
    for (index, element) in interpreter.output().iter().enumerate() {
        match element {
            Element::Text(text) => {
                match (&last, !text.is_empty()) {
                    (Empty, true) => {
                        ret += text;
                        last = Text;
                    }
                    (Text, true) => {
                        ret += "\n";
                        ret += text;
                    }
                    (Text, false) => last = Break,
                    (Break, true) => {
                            ret += "\n\n";
                            ret += text;
                            last = Text;
                    }
                    _ => {}
                }
            }
            Element::Link(title, _)
            | Element::ContentLink(title, _)
            | Element::JumpLink(title, _, _) => {
                match last {
                    Text => ret += "\n",
                    Break => ret += "\n\n",
                    _ => {}
                };
                last = Text;
                ret += &format!("{}. [{}]", input_id, title);
                input_id += 1;
                choices.push(UserActions::Tap(index));
            }
            Element::Input(_, _) => {
                match last {
                    Text => ret += "\n",
                    Break => ret += "\n\n",
                    _ => {}
                };
                last = Text;
                ret += &format!("{}. [__________]", input_id);
                input_id += 1;
                choices.push(UserActions::Prompt(index));
            }
            Element::Error(e) => ret += &format!("ERROR: {}\n", e),
        }
    }
    return (ret, choices);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("lift {} - A Language for Interactive Fiction Texts", env!("CARGO_PKG_VERSION"));
        println!("Usage: {} <FILES>", args[0]);
        process::exit(1);
    }

    let story = create_story(&args[1..]);
    let mut interpreter = Interpreter::new(story);
    play(&mut interpreter);
    process::exit(0);
}

fn create_story(files: &[String]) -> Story {
    let mut source = String::new();
    for file in files {
        if let Ok(content) = fs::read_to_string(file) {
            source.push_str(&content);
        } else {
            eprintln!("Error while reading file '{}'", file);
            process::exit(0);
        }
    }
    match Story::new(&source) {
        Ok(story) => return story,
        Err(error) => {
            eprintln!("{}", error);
            process::exit(1);
        }
    }
}
