use regex::Regex;
use std::fmt;
use lazy_static::lazy_static;
use crate::content::{TextElement, TextContent};
use crate::expression::{ExpressionParser, Expression};

pub enum ParserResult<Token, Error, Size> {
    Some(Token, Size), // Token and size to consume
    None(Size), // Consume but don't return a token
    End(Size), // End of parsing
    Error(Error) // Fail
}

pub trait Parser {
    type Token;
    type Error;

    fn next(&mut self, string: &str) -> ParserResult<Self::Token, Self::Error, usize>;

    fn parse(&mut self, string: &str) -> (Vec<Self::Token>, usize, Option<Self::Error>) {
        let mut size: usize = 0;
        let mut tokens = Vec::<Self::Token>::new();
        loop {
            match self.next(&string[size..]) {
                ParserResult::Some(token, len) => {
                    tokens.push(token);
                    size += len;
                }
                ParserResult::None(len) => {
                    size += len;
                }
                ParserResult::End(len) => return (tokens, size + len, None),
                ParserResult::Error(error) => return (tokens, size, Some(error))
            }
        }
    }
}

pub struct TextParser {
    pub expects: String,
}

impl TextParser {
    fn literal(mut elements: Vec<TextElement>, literal: String, size: usize)
    -> ParserResult<TextContent, TextParserError, usize> {
        let trimmed = literal.trim_end();
        if !trimmed.is_empty() {
            elements.push(TextElement::Text(trimmed.to_string()));
        }
        return ParserResult::Some(TextContent{elements}, size);
    }

    fn literal_or_none(elements: Vec<TextElement>, literal: String, size: usize)
    -> ParserResult<TextContent, TextParserError, usize> {
        let trimmed = literal.trim_end();
        if elements.is_empty() && trimmed.is_empty() {
            return ParserResult::None(size);
        }
        return Self::literal(elements, literal, size);
    }
}

pub enum TextParserError {
}

impl Parser for TextParser {
    type Token = TextContent;
    type Error = TextParserError;

    fn next(&mut self, string: &str) -> ParserResult<Self::Token, Self::Error, usize> {
        let mut elements: Vec::<TextElement> = vec![];
        lazy_static! {
            static ref VARIABLE_REGEX: Regex = Regex::new(r"^[a-zA-Z_]\w*").unwrap();
            static ref COMMAND_REGEX: Regex = Regex::new(r"^([a-z_]+|@)").unwrap();
            static ref WHITESPACE_REGEX: Regex = Regex::new(r"^[^\S\n]*").unwrap();
        }
        let mut chars = string.chars();
        let mut literal = String::new();
        let mut consumed_size: usize = 0;
        if let Some(capture) = WHITESPACE_REGEX.captures(string) {
            let size = capture.get(0).unwrap().as_str().len();
            consumed_size += size;
            chars = string[size..].chars();
        }
        while let Some(c) = chars.next() {
            if c == '\\' {
                consumed_size += '\\'.len_utf8();
                let next = chars.next();
                let replace = match next {
                    Some('\n') => ' ',
                    Some('n') => '\n',
                    Some(c) => c,
                    None => '\\'
                };
                if let Some('\n') = next {
                    if let Some(capture) = WHITESPACE_REGEX.captures(chars.as_str()) {
                        let size = capture.get(0).unwrap().as_str().len();
                        consumed_size += size;
                        chars = chars.as_str()[size..].chars();
                    }
                }
                literal.push(replace);
                consumed_size += replace.len_utf8();
            }
            else if c == '$' {
                if let Some(capture) = VARIABLE_REGEX.captures(chars.as_str()) {
                    if !literal.is_empty() {
                        elements.push(TextElement::Text(literal));
                        literal = "".to_string();
                    }
                    let variable = capture.get(0).unwrap().as_str();
                    let size = '$'.len_utf8() + variable.len();
                    chars = chars.as_str()[variable.len()..].chars();
                    consumed_size += size;
                    elements.push(TextElement::Variable(variable.to_string()));
                }
                else if chars.as_str().starts_with('{') {
                    if !literal.is_empty() {
                        elements.push(TextElement::Text(literal));
                        literal = "".to_string();
                    }
                    consumed_size += "${".len();
                    chars.next();
                    let expression_string = chars.as_str();
                    let (expression, len) = ExpressionParser::parse(expression_string);
                    let rest = chars.as_str()[len..].trim_start();
                    if rest.starts_with("}") {
                        if let Ok(expr) = expression {
                            let size = '}'.len_utf8() + (expression_string.len() - rest.len());
                            chars = rest['}'.len_utf8()..].chars();
                            consumed_size += size;
                            elements.push(TextElement::Expression(expr));
                        }
                    }
                    else {
                        literal.push_str("${");
                    }
                } else {
                    literal.push('$');
                    consumed_size += '$'.len_utf8();
                }
            }
            else if c == '@' {
                if let Some(_) = COMMAND_REGEX.captures(chars.as_str()) {
                    return Self::literal_or_none(elements, literal, consumed_size);
                }
                else {
                    literal.push('@');
                    consumed_size += '@'.len_utf8();
                }
            }
            else if c == '\n' {
                consumed_size += '\n'.len_utf8();
                return Self::literal(elements, literal, consumed_size);
            }
            else if !self.expects.is_empty() && string[consumed_size..].starts_with(&self.expects) {
                return Self::literal_or_none(elements, literal, consumed_size);
            }
            else {
                literal.push(c);
                consumed_size += c.len_utf8();
            }
        }
        return Self::literal(elements, literal, consumed_size);
    }
}

pub enum ContentError {
    InvalidCommand(String),
    InvalidParameters(String),
    MissingClosingBrace
}

impl fmt::Display for ContentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContentError::InvalidCommand(cmd) => write!(f, "Invalid command: @{}", cmd),
            ContentError::InvalidParameters(cmd) => write!(f, "Invalid parameters for command: @{}", cmd),
            ContentError::MissingClosingBrace => write!(f, "Missing closing brace")
        }
    }
}

pub struct ContentParser {
    capture_level: usize
}

impl ContentParser {
    pub fn new() -> Self {
        ContentParser { capture_level: 0 }
    }
}

pub enum ContentToken {
    Text(TextContent),
    Command(String, Vec<Params>),
    BlockEnd
}

impl Parser for ContentParser {
    type Token = ContentToken;
    type Error = ContentError;
    
    fn next(&mut self, string: &str) -> ParserResult<Self::Token, Self::Error, usize> {
        if string.is_empty() {
            return match self.capture_level {
                0 => ParserResult::End(0),
                _ => ParserResult::Error(Self::Error::MissingClosingBrace)
            }
        }
        lazy_static! {
            static ref COMMENT_REGEX: Regex = Regex::new(r"^@@.*\n").unwrap();
            static ref COMMAND_REGEX: Regex = Regex::new(r"^@(?P<name>[a-z_]+)").unwrap();
            static ref COMMAND_END_REGEX: Regex = Regex::new(r"^[^\S\n]*(\n|)").unwrap();
        }
        let mut slice = string;

        if let Some(capture) = COMMENT_REGEX.captures(slice) {
            let comment = capture.get(0).unwrap().as_str();
            slice = &slice[comment.len()..];
        }

        let expected_token = match self.capture_level {
            0 => "".to_string(),
            _ => "}".to_string()
        };
        let mut parser = TextParser{ expects: expected_token.to_string() };
        
        let next_token = parser.next(slice);
        if let ParserResult::Some(text_content, text_size) = next_token {
            slice = &slice[text_size..];
            return ParserResult::Some(Self::Token::Text(text_content), string.len() - slice.len());
        }
        else if let ParserResult::None(text_size) = next_token {
            slice = &slice[text_size..];
        }
        if let Some(capture) = COMMAND_REGEX.captures(slice) {
            let command_name = capture.name("name").unwrap().as_str();
            let command_size = capture.get(0).unwrap().as_str().len();
            slice = &slice[command_size..];

            let expect: Vec<Expect> = match command_name {
                "link" => vec![
                    Expect::Or(vec![
                        vec![Expect::Text, Expect::string("->"), Expect::Text, Expect::Block],
                        vec![Expect::Text, Expect::string("->"), Expect::Text],
                        vec![Expect::Text, Expect::Block]
                    ])
                ],
                "input" => vec![
                    Expect::Variable, Expect::Block
                ],
                "set" | "setlocal" => vec![
                    Expect::Indices, Expect::string("="), Expect::Expression
                ],
                "if" | "elseif" => vec![
                    Expect::Expression, Expect::Block
                ],
                "else" => vec![
                    Expect::Block
                ],
                "for" => vec![
                    Expect::Or(vec![
                        vec![Expect::Variable, Expect::string(","), Expect::Variable],
                        vec![Expect::Variable]
                    ]),
                    Expect::string("in"), Expect::Expression, Expect::Block
                ],
                "while" => vec![
                    Expect::Expression, Expect::Block
                ],
                "goto" | "import" => vec![
                    Expect::Text
                ],
                _ => return ParserResult::Error(Self::Error::InvalidCommand(command_name.to_string()))
            };
            if let Some(params) = Params::expect(&mut slice, &expect, &expected_token) {
                if let Some(Params::Block) = params.last() {
                    self.capture_level += 1;
                }
                if let Some(capture) = COMMAND_END_REGEX.captures(slice) {
                    let size = capture.get(0).unwrap().as_str().len();
                    slice = &slice[size..];
                }
                let final_size = string.len() - slice.len();
                return ParserResult::Some(Self::Token::Command(command_name.to_string(), params), final_size);
            }
            else {
                return ParserResult::Error(Self::Error::InvalidParameters(command_name.to_string()))
            }
        }

        if self.capture_level > 0 && slice.starts_with("}") {
            self.capture_level -= 1;
            slice = &slice["}".len()..];
            if let Some(capture) = COMMAND_END_REGEX.captures(slice) {
                let size = capture.get(0).unwrap().as_str().len();
                slice = &slice[size..];
            }
            let final_size = string.len() - slice.len();
            return ParserResult::Some(Self::Token::BlockEnd, final_size);
        }
        return ParserResult::End(0);
    }
}

enum Expect {
    Text,
    Variable,
    Indices,
    Prefix(String),
    Or(Vec<Vec<Expect>>),
    Expression,
    Block
}

impl Expect {
    fn string(string: &str) -> Self {
        Self::Prefix(string.to_string())
    }
}


pub enum Params {
    Text(TextContent),
    Variable(String),
    Indices(String, Vec<Expression>),
    Expression(Expression),
    Block
}

impl Params {
    fn expect(slice: &mut &str, parameters: &Vec<Expect>, expect_text: &str) -> Option<Vec<Params>> {
        lazy_static! {
            static ref VARIABLE_REGEX: Regex = Regex::new(r"^(?P<variable>[a-zA-Z_]\w*)").unwrap();
        }
        let mut response = Vec::<Params>::new();
        for (index, param) in parameters.iter().enumerate() {
            *slice = &slice.trim_start();
            match param {
                Expect::Or(params_list) => {
                    let mut complete = false;
                    for params in params_list {
                        let mut first_pass = *slice;
                        if let Some(mut sub_response) = Params::expect(&mut first_pass, params, expect_text) {
                            response.append(&mut sub_response);
                            *slice = first_pass;
                            complete = true;
                            break;
                        }
                    }
                    if !complete { return None }
                }
                Expect::Text => {
                    let expects = match parameters.get(index + 1) {
                        Some(Expect::Block) => "{",
                        Some(Expect::Prefix(s)) => s,
                        _ => expect_text
                    };

                    let mut parser = TextParser { expects: expects.to_string() };
                    let text_token = parser.next(slice);
                    if let ParserResult::Some(content, size) = text_token {
                        *slice = &slice[size..];
                        response.push(Params::Text(content));
                    }
                    else { return None }
                }
                Expect::Variable => {
                    if let Some(capture) = VARIABLE_REGEX.captures(slice) {
                        let variable = capture.name("variable").unwrap().as_str();
                        *slice = &slice[variable.len()..];
                        response.push(Params::Variable(variable.to_string()));
                    }
                    else { return None }
                }
                Expect::Indices => {
                    let mut parser = ExpressionParser::new();
                    let (tokens, size, error) = parser.parse(slice);
                    if let Some(error) = error {
                        return None
                    }
                    if let Some((variable, indices)) = ExpressionParser::parse_indices(tokens) {
                        *slice = &slice[size..];
                        response.push(Params::Indices(variable, indices));
                    }
                    else { return None }
                }
                Expect::Prefix(s) => {
                    if slice.starts_with(s) {
                        *slice = &slice[s.len()..];
                    } 
                    else { return None }
                }
                Expect::Block => {
                    if slice.starts_with("{") {
                        response.push(Params::Block);
                        *slice = &slice["}".len()..];
                    }
                    else { return None }
                }
                Expect::Expression => {
                    if let (Ok(expression), size) = ExpressionParser::parse(slice) {
                        response.push(Params::Expression(expression));
                        *slice = &slice[size..];
                    }
                    else { return None }
                }
            }
        }
        return Some(response);
    }
}
