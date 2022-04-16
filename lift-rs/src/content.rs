// Content Parser
use crate::expression::*;
use crate::parser::{ContentParser, Parser, ContentToken, Params, ContentError};

pub enum Content {
    Text(TextContent),
    Link(Action),
    Input { variable: String, page: String, action: usize },
    Set { local: bool, variable: String, indices: Vec<Expression>, expression: Expression },
    If { expression: Expression, content: Vec<Content> },
    ElseIf { expression: Expression, content: Vec<Content> },
    Else { content: Vec<Content> },
    For { index: Option<String>, variable: String, expression: Expression, content: Vec<Content> },
    While { expression: Expression, content: Vec<Content> },
    Goto(TextContent),
    Import(TextContent),
    Error(String)
}

impl Content {
    fn make_error(message: &str) -> Content {
        println!("{}", message);
        Content::Error(message.to_string())
    }
}

pub enum Action {
    Normal { title: TextContent, destination: TextContent },
    Content { title: TextContent, page: String, action: usize },
    JumpLink { title: TextContent, destination: TextContent, page: String, action: usize}
    //Input { title: String, variable: String, content: Vec<Content> },
    //Choice { title: String, variable: String, choices: String, content: Vec<Content> }
}

pub struct Page {
    pub content: Vec<Content>,
    pub actions: Vec<Vec<Content>>
}

impl Page {
    pub fn parse(title: &str, source: &str) -> Result<Page, (usize, ContentError)> {
        let mut content_stack: Vec<Vec<Content>> = vec![vec![]];
        let mut command_stack: Vec<(String, Vec<Params>)> = vec![];
        let mut actions: Vec<Vec<Content>> = vec![];
        let mut parser = ContentParser::new();
        let (tokens, size, error_option) = parser.parse(source);
        if let Some(error) = error_option {
            return Err((size, error));
        }

        for token in tokens {
            match token {
                ContentToken::Text(text) => content_stack.last_mut().unwrap().push(Content::Text(text)),
                ContentToken::Command(name, mut params) => {
                    if let Some(Params::Block) = params.last() {
                        params.pop();
                        content_stack.push(vec![]);
                        command_stack.push((name, params));
                    } else {
                        content_stack.last_mut().unwrap().push(Content::build_command(name, params, None, title, &mut actions));
                    }
                }
                ContentToken::BlockEnd => {
                    let block_contents = content_stack.pop();
                    let (name, params) = command_stack.pop().unwrap();
                    content_stack.last_mut().unwrap().push(
                        Content::build_command(name, params, block_contents, title, &mut actions)
                    ); 
                }
            }
        }
        if command_stack.len() > 0 {
            return Err((size, ContentError::MissingClosingBrace));
        }
        return Ok(Page { content: content_stack.pop().unwrap(), actions });
    }
}

enum Args {
    Invalid,
    Nothing,
    One(Params),
    Two(Params, Params),
    Three(Params, Params, Params)
}

impl Args {
    fn from_params(mut params: Vec<Params>) -> Self {
        match params.len() {
            0 => Args::Nothing,
            1 => Args::One(params.remove(0)),
            2 => Args::Two(params.remove(0), params.remove(0)),
            3 => Args::Three(params.remove(0), params.remove(0), params.remove(0)),
            _ => Args::Invalid
        }
    }
}

impl Content {
    fn build_command(name: String, params: Vec<Params>, block: Option<Vec<Content>>, page: &str, actions: &mut Vec<Vec<Content>>) -> Content {
        let args = Args::from_params(params);
        match (name.as_str(), args, block) {
            ("link", Args::Two(Params::Text(title), Params::Text(destination)), Some(content)) => {
                let action = actions.len();
                actions.push(content);
                Content::Link(Action::JumpLink{title, destination, page: page.to_string(), action})
            }
            ("link", Args::Two(Params::Text(title), Params::Text(destination)), None) => {
                Content::Link(Action::Normal{title, destination})
            }
            ("link", Args::One(Params::Text(title)), Some(content)) => {
               let action = actions.len();
               actions.push(content);
               Content::Link(Action::Content{title, page: page.to_string(), action})
            }
            ("input", Args::One(Params::Variable(variable)), Some(content)) => {
                let action = actions.len();
                actions.push(content);
                Content::Input{variable, page: page.to_string(), action}
            }
            ("goto", Args::One(Params::Text(page)), None) => Content::Goto(page),
            ("import", Args::One(Params::Text(page)), None) => Content::Import(page),
            ("set", Args::Two(Params::Indices(variable, indices), Params::Expression(expression)), None) => {
                Content::Set { local: false, variable, indices, expression }
            }
            ("setlocal", Args::Two(Params::Indices(variable, indices), Params::Expression(expression)), None) => {
                Content::Set { local: true, variable, indices, expression }
            }
            ("if", Args::One(Params::Expression(expression)), Some(content)) => {
                Content::If { expression, content }
            }
            ("elseif", Args::One(Params::Expression(expression)), Some(content)) => {
                Content::ElseIf { expression, content }
            }
            ("else", Args::Nothing, Some(content)) => {
                Content::Else { content }
            }
            ("for", Args::Three(Params::Variable(index), Params::Variable(variable), Params::Expression(expression)), Some(content)) => {
                Content::For { index: Some(index), variable, expression, content }
            }
            ("for", Args::Two(Params::Variable(variable), Params::Expression(expression)), Some(content)) => {
                Content::For { index: None, variable, expression, content }
            }
            ("while", Args::One(Params::Expression(expression)), Some(content)) => {
                Content::While { expression, content }
            }
            _ => Content::make_error(&format!("Invalid command: {}", name))
        }
    }
}


pub enum TextElement {
    Text(String),
    Variable(String),
    Expression(Expression)
}

pub struct TextContent {
    pub elements: Vec<TextElement>
}

impl TextContent {
    pub fn eval(&self, state: &impl StateManager) -> String {
        return self.elements.iter().map(|element| {
            match element {
                TextElement::Text(s) => s.to_string(),
                TextElement::Variable(var) => state.get(&var).map_or("null".to_string(), |x| x.to_string()),
                TextElement::Expression(expr) => expr.eval(state).to_string()
            }
        }).collect::<Vec<_>>().join("");
    }
}
