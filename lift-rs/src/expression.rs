use crate::parser::Parser;
use crate::parser::ParserResult;
use std::collections::HashMap;
use crate::value::{Value, Value::*, comparison, operator};
use lazy_static::lazy_static;
use regex::Regex;

pub trait StateManager {
    fn get(&self, variable: &str) -> Option<&Value>;
}

#[derive(Clone, PartialEq)]
pub enum Operator {
    Add, Sub, Mul, Div, Rem, Exp,
    And, Or,
    Contains, In,
    Equal, NotEqual,
    GreaterOrEqual, Greater, LessOrEqual, Less,
    Index,
    Coalesce
}

use Operator::*;

impl Operator {
    fn precedence(&self) -> u32 {
        match self {
            Exp => 80,
            Coalesce => 70,
            Index => 60,
            Mul | Div | Rem => 50,
            Add | Sub => 40,
            Contains | In => 30,
            Equal | NotEqual | GreaterOrEqual | Greater | LessOrEqual | Less => 20,
            And => 10,
            Or => 0
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            Exp => false,
            _ => true
        }
    }

    fn apply(&self, a: Value, b: Value) -> Value {
        match self {
            Exp => a.pow(&b),
            Add => a + b,
            Mul => a * b,
            Sub => a - b,
            Div => a / b,
            Rem => a % b,
            And => Boolean(a.is_true() & b.is_true()),
            Or => Boolean(a.is_true() | b.is_true()),
            Contains => operator::contains(&a, &b),
            In => operator::contains(&b, &a),
            Index => operator::index(&a, &b),
            Equal => Boolean(a == b),
            NotEqual => Boolean(a != b),
            Greater => comparison::gt(&a, &b),
            GreaterOrEqual => comparison::gte(&a, &b),
            Less => comparison::lt(&a, &b),
            LessOrEqual => comparison::lte(&a, &b),
            Coalesce => a.coalesce(&b),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum UnaryOperator {
    Minus, Plus, Not
}
use UnaryOperator::*;

impl UnaryOperator {
    fn precedence(&self) -> u32 {
        match self {
            Minus | Plus | Not => 6
        }
    }

    fn apply(&self, a: Value) -> Value {
        match self {
            Minus => -a,
            Not => Boolean(!a.is_true()),
            Plus => a,
        }
    }
}


pub enum ExpressionToken {
    Constant(Value),
    Operator(Operator),
    UnaryOperator(UnaryOperator),
    Function(String, usize),
    Array(usize),
    Object(usize),
    Variable(String)
}

pub struct Expression {
    pub tokens: Vec<ExpressionToken>
}

impl Expression {
    pub fn eval(&self, state: &impl StateManager) -> Value {
        let mut value_stack = Vec::<Value>::new();

        for token in self.tokens.iter() {
            match token {
                ExpressionToken::Constant(value) => value_stack.push(value.clone()),
                ExpressionToken::Variable(var) => {
                    value_stack.push(state.get(var).map_or(Null, |v| v.clone()))
                },
                ExpressionToken::Operator(op) => {
                    if value_stack.len() < 2 {
                        return Null;
                    }
                    let b = value_stack.pop().unwrap();
                    let a = value_stack.pop().unwrap();
                    value_stack.push(op.apply(a, b));
                },
                ExpressionToken::UnaryOperator(op) => {
                    match value_stack.pop() {
                        Some(a) => value_stack.push(op.apply(a)),
                        None => return Null
                    }
                },
                ExpressionToken::Array(elements) => {
                    if value_stack.len() < *elements {
                        return Null;
                    }
                    let element_stack = value_stack.split_off(value_stack.len() - elements);
                    value_stack.push(Array(element_stack));
                }
                ExpressionToken::Object(elements) => {
                    if value_stack.len() < (*elements) * 2 {
                        return Null;
                    }
                    let mut obj = HashMap::<String, Value>::new();
                    for _ in 0..*elements {
                        let value = value_stack.pop();
                        let key = value_stack.pop();
                        if let (Some(k), Some(v)) = (key, value) {
                            obj.insert(k.to_string(), v);
                        }
                    }
                    value_stack.push(Object(obj));
                }
                ExpressionToken::Function(function, arguments) => {
                    if value_stack.len() < *arguments {
                        return Null;
                    }
                    let argument_stack = value_stack.split_off(value_stack.len() - arguments);
                    value_stack.push(Value::eval_function(function, argument_stack));
                }
            }
        }
        return value_stack.pop().unwrap_or(Null);
    }

    pub fn constant(value: Value) -> Self {
        Expression { tokens: vec![ExpressionToken::Constant(value)] }
    }
}

// Parser
#[derive(Clone, PartialEq)]
pub enum ParserToken {
    LeftParen,   RightParen,
    ArrayStart,  ArrayEnd,
    ObjectStart, ObjectEnd,
    IndexStart,  IndexEnd,
    Separator,
    ObjectSeparator,
    Function(String),
    Constant(Value),
    Operator(Operator),
    ObjectIndex(String),
    UnaryOperator(UnaryOperator),
    Variable(String)
}

impl ParserToken {
    fn is_operator(&self) -> bool {
        match self {
            ParserToken::Operator(_) => true,
            ParserToken::UnaryOperator(_) => true,
            _ => false
        }
    }

    fn to_expression_operator(self) -> Option<ExpressionToken> {
        match self {
            ParserToken::Operator(op) => Some(ExpressionToken::Operator(op)),
            ParserToken::UnaryOperator(op) => Some(ExpressionToken::UnaryOperator(op)),
            _ => None
        }
    }
}

enum ListType {
    Function(String),
    Array,
    Object
}

impl ListType {
    fn is_function(&self) -> bool {
        match self {
            ListType::Function(_) => true,
            _ => false
        }
    }
}

pub struct ExpressionParser {
    last: Option<ParserToken>,
    token_stack: Vec<ParserToken>,
    size: usize
}

impl ExpressionParser {
    fn get_token(&mut self, regex: &Regex, search: &str) -> Option<String> {
        if let Some(capture) = regex.captures(search) {
            // Consume the entire regex match or only the first capture group.
            let token_string = capture.get(1).unwrap_or(capture.get(0).unwrap()).as_str();
            self.size += token_string.len();
            return Some(token_string.to_string());
        }
        return None;
    }

    pub fn new() -> Self {
        return Self { last: None, token_stack: Vec::new(), size: 0};
    }

    pub fn parse_indices(tokens: Vec<ParserToken>) -> Option<(String, Vec<Expression>)> {
        let variable_name: String;
        let mut token_stack = Vec::<ParserToken>::new();
        let mut expression_stack = Vec::<Expression>::new();

        let mut bracket_counter: usize = 0;
        if let Some(ParserToken::Variable(name)) = tokens.first() {
            variable_name = name.to_string();
        } else {
            return None
        }

        for token in tokens[1..].to_vec() {
            match (&token, bracket_counter) {
                (ParserToken::IndexStart, 0) => bracket_counter += 1,
                (ParserToken::IndexStart, _) => {
                    bracket_counter += 1;
                    token_stack.push(token);
                }
                (ParserToken::IndexEnd, 1) => {
                    if let Ok(expression) = Self::convert_to_postfix(token_stack) {
                        expression_stack.push(expression);
                    }
                    token_stack = Vec::<ParserToken>::new();
                }
                (ParserToken::IndexEnd, _) => {
                    token_stack.push(token);
                    bracket_counter -= 1;
                }
                (ParserToken::ObjectIndex(key), 0) => {
                    expression_stack.push(Expression::constant(Text(key.to_string())));
                }
                (_, 0) => return None,
                (_, _) => token_stack.push(token)
            }
        }

        if !token_stack.is_empty() {
            return None
        }
        return Some((variable_name, expression_stack));
    }

    fn pop_while<F>(operator_stack: &mut Vec<ParserToken>, expression: &mut Vec<ExpressionToken>, condition: F) where F: Fn(&ParserToken) -> bool {
        while operator_stack.last().map_or(false, &condition) {
            let operator = operator_stack.pop().unwrap();
            expression.push(operator.to_expression_operator().unwrap());
        }
    }

    fn convert_to_postfix(tokens: Vec<ParserToken>) -> Result<Expression, ParsingError> {
        let mut operator_stack = Vec::<ParserToken>::new();
        let mut function_stack = Vec::<(usize, ListType)>::new();
        let mut return_expression = Vec::<ExpressionToken>::new();
    
        let mut previous_token: Option<ParserToken> = None;
        for token_reference in tokens.iter() {
            let token = token_reference.clone();
            match token {
                ParserToken::LeftParen | ParserToken::IndexStart => {
                    operator_stack.push(token);
                }
                ParserToken::RightParen => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &ParserToken::LeftParen);
                    if let None = operator_stack.pop() {
                        return Err(ParsingError::MismatchedParentheses);
                    }
                    if function_stack.last().map_or(false, |x| x.1.is_function()) {
                        if let Some((mut arg_count, ListType::Function(name))) = function_stack.pop() {
                            if let Some(ParserToken::LeftParen) = previous_token {
                                arg_count = 0;
                            }
                            return_expression.push(ExpressionToken::Function(name, arg_count));
                        }
                    }
                }
                ParserToken::ObjectSeparator => {
                }
                ParserToken::Separator => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x.is_operator());
                    if let Some((arg_count, list_type)) = function_stack.pop() {
                        function_stack.push((arg_count + 1, list_type));
                    }
                    else {
                        return Err(ParsingError::InvalidSeparatorToken);
                    }
                }
                ParserToken::ArrayStart => {
                    operator_stack.push(token);
                    function_stack.push((1, ListType::Array));
                }
                ParserToken::ObjectStart => {
                    operator_stack.push(token);
                    function_stack.push((1, ListType::Object));
                }
                ParserToken::ObjectEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &ParserToken::ObjectStart);
                    if let Some(ParserToken::ObjectStart) = operator_stack.pop() {
                        if let Some((mut arg_count, ListType::Object)) = function_stack.pop() {
                            if let Some(ParserToken::ObjectStart) = previous_token {
                                arg_count = 0;
                            }
                            return_expression.push(ExpressionToken::Object(arg_count));
                        }
                    }
                }
                ParserToken::ArrayEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &ParserToken::ArrayStart);
                    if let Some(ParserToken::ArrayStart) = operator_stack.pop() {
                        if let Some((mut arg_count, ListType::Array)) = function_stack.pop() {
                            if let Some(ParserToken::ArrayStart) = previous_token {
                                arg_count = 0;
                            }
                            return_expression.push(ExpressionToken::Array(arg_count));
                        }
                        else {
                            return Err(ParsingError::MismatchedBrackets);
                        }
                    }
                }
                ParserToken::IndexEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &ParserToken::IndexStart);
                    if let Some(ParserToken::IndexStart) = operator_stack.pop() {
                        return_expression.push(ExpressionToken::Operator(Operator::Index));
                    }
                    else {
                        return Err(ParsingError::MismatchedBrackets);
                    }
                }
                ParserToken::Function(name) => {
                    let list = (1, ListType::Function(name));
                    function_stack.push(list);
                }
                ParserToken::UnaryOperator(_) => {
                    operator_stack.push(token);
                }
                ParserToken::Operator(ref op) => {
                    let precedence = op.precedence();
                    Self::pop_while(&mut operator_stack, &mut return_expression, |last| -> bool {
                        let last_precedence = match last {
                            ParserToken::Operator(op) => op.precedence(),
                            ParserToken::UnaryOperator(op) => op.precedence(),
                            _ => return false
                        };
                        return (last_precedence > precedence) || ((last_precedence == precedence) && op.is_left_associative());
                    });
                    operator_stack.push(token);
                }
                ParserToken::ObjectIndex(index) => {
                    return_expression.push(ExpressionToken::Constant(Text(index)));
                    return_expression.push(ExpressionToken::Operator(Operator::Index));
                }
                ParserToken::Variable(var) => {
                    return_expression.push(ExpressionToken::Variable(var.to_string()));
                }
                ParserToken::Constant(value) => {
                    return_expression.push(ExpressionToken::Constant(value.clone()));
                }
            }
            previous_token = Some(token_reference.clone());
        }
        while let Some(token) = operator_stack.pop() {
            return_expression.push(token.to_expression_operator().unwrap());
        }
        return Ok(Expression { tokens: return_expression });
    }

    pub fn parse(string: &str) -> (Result<Expression, ParsingError>, usize) {
        let mut parser = Self::new();
        let (tokens, size, error) = parser.parse(string);
        if let Some(error) = error {
            return (Err(error), size);
        }
        return (Self::convert_to_postfix(tokens), size);
    }
}


pub enum ParsingError {
    MismatchedBraces,
    MismatchedParentheses,
    MismatchedBrackets,
    InvalidSeparatorToken
}

impl Parser for ExpressionParser {
    type Token = ParserToken;
    type Error = ParsingError;

    fn next(&mut self, string: &str) -> ParserResult<ParserToken, ParsingError, usize> {
        let mut size = 0;
        let mut token: Option<ParserToken> = None;
        lazy_static! {
        static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s+").unwrap();
        static ref LEFT_PAREN_REGEX: Regex = Regex::new(r"^\(").unwrap();
        static ref RIGHT_PAREN_REGEX: Regex = Regex::new(r"^\)").unwrap();
        static ref ARRAY_START_REGEX: Regex = Regex::new(r"^\[").unwrap();
        static ref OBJECT_START_REGEX: Regex = Regex::new(r"^\{").unwrap();
        static ref OBJECT_END_REGEX: Regex = Regex::new(r"^\}").unwrap();
        static ref ARRAY_END_REGEX: Regex = Regex::new(r"^\]").unwrap();
        static ref SEPARATOR_REGEX: Regex = Regex::new(r"^,").unwrap();
        static ref OBJECT_SEPARATOR_REGEX: Regex = Regex::new(r"^:").unwrap();

        static ref BOOLEAN_REGEX: Regex = Regex::new(r"^(true|false)([^\w]+|$)").unwrap();
        static ref STRING_REGEX: Regex = Regex::new(r#"^("((\\.|[^\\\n"])*)")"#).unwrap();
        static ref FLOAT_REGEX: Regex = Regex::new(r"^(\d+(\.\d+[Ee][+-]?\d+|[Ee][+-]?\d+|\.\d+))").unwrap();
        static ref INTEGER_REGEX: Regex = Regex::new(r"^\d+").unwrap();
        static ref NULL_REGEX: Regex = Regex::new(r"^(null)([^\w]+|$)").unwrap();

        static ref CONTAINS_REGEX: Regex = Regex::new(r"(contains)[^\w]+").unwrap();
        static ref IN_REGEX: Regex = Regex::new(r"^(in)[^\w]+").unwrap();
        static ref AND_REGEX: Regex = Regex::new(r"^(and)[^\w]+").unwrap();
        static ref OR_REGEX: Regex = Regex::new(r"^(or)[^\w]+").unwrap();
        static ref NOT_REGEX: Regex = Regex::new(r"^(not)[^\w]+").unwrap();

        static ref FUNCTION_REGEX: Regex = Regex::new(r"^([a-zA-Z_]+)\s*\(").unwrap();
        static ref VARIABLE_REGEX: Regex = Regex::new(r"^([a-zA-Z_]\w*)").unwrap();

        static ref EQ_REGEX: Regex = Regex::new(r"^==").unwrap();
        static ref NEQ_REGEX: Regex = Regex::new(r"^!=").unwrap();
        static ref GTE_REGEX: Regex = Regex::new(r"^>=").unwrap();
        static ref GT_REGEX: Regex = Regex::new(r"^>").unwrap();
        static ref LTE_REGEX: Regex = Regex::new(r"^<=").unwrap();
        static ref LT_REGEX: Regex = Regex::new(r"^<").unwrap();

        static ref POW_REGEX: Regex = Regex::new(r"^\^").unwrap();
        static ref INDEX_REGEX: Regex = Regex::new(r"^(\.[a-zA-Z_]\w*)").unwrap();
        static ref MOD_REGEX: Regex = Regex::new(r"^%").unwrap();

        static ref PLUS_REGEX: Regex = Regex::new(r"^\+").unwrap();
        static ref MINUS_REGEX: Regex = Regex::new(r"^-").unwrap();
        static ref MUL_REGEX: Regex = Regex::new(r"^\*").unwrap();

        static ref DIV_REGEX: Regex = Regex::new(r"^/").unwrap();
        static ref COALESCE_REGEX: Regex = Regex::new(r"^\?\?").unwrap();
        static ref END_REGEX: Regex = Regex::new(r"^;").unwrap();
        }

        if let Some(whitespace) = WHITESPACE_REGEX.captures(&string) {
            size += whitespace.get(0).unwrap().as_str().len();
        }
        self.size = size;
        let slice = &string[size..];

        if let Some(_) = self.get_token(&LEFT_PAREN_REGEX, slice) {
            token = Some(ParserToken::LeftParen);
            self.token_stack.push(ParserToken::LeftParen);
        }
        else if let Some(_) = self.get_token(&RIGHT_PAREN_REGEX, slice) {
            if let Some(ParserToken::LeftParen) = self.token_stack.pop() {
                token = Some(ParserToken::RightParen);
            }
        }
        else if let Some(_) = self.get_token(&ARRAY_START_REGEX, slice) {
            let start = match self.last {
                Some(ParserToken::RightParen) |
                Some(ParserToken::ArrayEnd) |
                Some(ParserToken::ObjectEnd) |
                Some(ParserToken::IndexEnd) |
                Some(ParserToken::ObjectIndex(_)) |
                Some(ParserToken::Constant(_)) |
                Some(ParserToken::Variable(_)) => ParserToken::IndexStart,
                _ => ParserToken::ArrayStart
            };
            self.token_stack.push(start.clone());
            token = Some(start);
        }
        else if let Some(_) = self.get_token(&ARRAY_END_REGEX, slice) {
            token = match self.token_stack.pop() {
                Some(ParserToken::ArrayStart) => Some(ParserToken::ArrayEnd),
                Some(ParserToken::IndexStart) => Some(ParserToken::IndexEnd),
                _ => None
            };
        }
        else if let Some(_) = self.get_token(&OBJECT_START_REGEX, slice) {
            token = Some(ParserToken::ObjectStart);
            self.token_stack.push(ParserToken::ObjectStart);
        }
        else if let Some(_) = self.get_token(&OBJECT_END_REGEX, slice) {
            if let Some(ParserToken::ObjectStart) = self.token_stack.pop() {
                token = Some(ParserToken::ObjectEnd);
            }
        }
        else if let Some(_) = self.get_token(&SEPARATOR_REGEX, slice) {
            token = Some(ParserToken::Separator);
        }
        else if let Some(_) = self.get_token(&OBJECT_SEPARATOR_REGEX, slice) {
            if let Some(ParserToken::ObjectStart) = self.token_stack.last() {
                token = Some(ParserToken::ObjectSeparator);
            }
        }
        else if let Some(s) = self.get_token(&BOOLEAN_REGEX, slice) {
            token = Some(ParserToken::Constant(Boolean(s == "true")));
        }
        else if let Some(s) = self.get_token(&STRING_REGEX, slice) {
            let mut chars = s.chars();
            chars.next();
            chars.next_back();
            let mut new_string = String::new();
            while let Some(c) = chars.next() {
                if c == '\\' {
                    match chars.next() {
                        Some('\\') => new_string.push('\\'),
                        Some('n') => new_string.push('\n'),
                        Some('t') => new_string.push('\t'),
                        Some(c) => new_string.push(c),
                        None => break
                    }
                }
                else {
                    new_string.push(c);
                }
            }
            token = Some(ParserToken::Constant(Text(new_string)));
        }
        else if let Some(s) = self.get_token(&FLOAT_REGEX, slice) {
            let float = s.parse::<f64>().unwrap();
            token = Some(ParserToken::Constant(Float(float)));
        }
        else if let Some(s) = self.get_token(&INTEGER_REGEX, slice) {
            let integer = s.parse::<i64>().unwrap();
            token = Some(ParserToken::Constant(Integer(integer)));
        }
        else if let Some(_) = self.get_token(&NULL_REGEX, slice) {
            token = Some(ParserToken::Constant(Null));
        }
        else if let Some(_) = self.get_token(&CONTAINS_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Contains));
        }
        else if let Some(_) = self.get_token(&IN_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::In));
        }
        else if let Some(_) = self.get_token(&AND_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::And));
        }
        else if let Some(_) = self.get_token(&OR_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Or));
        }
        else if let Some(_) = self.get_token(&NOT_REGEX, slice) {
            token = Some(ParserToken::UnaryOperator(UnaryOperator::Not));
        }
        else if let Some(name) = self.get_token(&FUNCTION_REGEX, slice) {
            token = Some(ParserToken::Function(name));
        }
        else if let Some(var) = self.get_token(&VARIABLE_REGEX, slice) {
            token = Some(ParserToken::Variable(var));
        }
        else if let Some(_) = self.get_token(&EQ_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Equal));
        }
        else if let Some(_) = self.get_token(&NEQ_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::NotEqual));
        }
        else if let Some(_) = self.get_token(&GTE_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::GreaterOrEqual));
        }        
        else if let Some(_) = self.get_token(&GT_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Greater));
        }
        else if let Some(_) = self.get_token(&LTE_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::LessOrEqual));
        }
        else if let Some(_) = self.get_token(&LT_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Less));
        }
        else if let Some(_) = self.get_token(&POW_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Exp));
        }
        else if let Some(s) = self.get_token(&INDEX_REGEX, slice) {
            let mut chars = s.chars();
            chars.next();
            let index = chars.as_str().to_string();
            token = Some(ParserToken::ObjectIndex(index));
        }
        else if let Some(_) = self.get_token(&MOD_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Rem));
        }
        else if let Some(_) = self.get_token(&MUL_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Mul));
        }
        else if let Some(_) = self.get_token(&DIV_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Div));
        }
        else if let Some(_) = self.get_token(&PLUS_REGEX, slice) {
            token = match self.last {
                None |
                Some(ParserToken::LeftParen) |
                Some(ParserToken::ArrayStart) |
                Some(ParserToken::Operator(_)) |
                Some(ParserToken::UnaryOperator(_)) => {
                    Some(ParserToken::UnaryOperator(UnaryOperator::Plus))
                },
                _ => Some(ParserToken::Operator(Operator::Add))
            };
        }
        else if let Some(_) = self.get_token(&MINUS_REGEX, slice) {
            token = match self.last {
                None | 
                Some(ParserToken::LeftParen) |
                Some(ParserToken::ArrayStart) |
                Some(ParserToken::Operator(_)) |
                Some(ParserToken::UnaryOperator(_)) => {
                    Some(ParserToken::UnaryOperator(UnaryOperator::Minus))
                },
                _ => Some(ParserToken::Operator(Operator::Sub))
            };
        }
        else if let Some(_) = self.get_token(&COALESCE_REGEX, slice) {
            token = Some(ParserToken::Operator(Operator::Coalesce));
        }
        else if let Some(_) = self.get_token(&END_REGEX, slice) {
            return ParserResult::End(self.size);
        }

        match (self.last.as_ref(), token.as_ref()) {
            (None, Some(ParserToken::Operator(_)))
            | (Some(ParserToken::IndexEnd), Some(ParserToken::ObjectStart))
            | (Some(ParserToken::RightParen), Some(ParserToken::LeftParen))
            | (Some(ParserToken::RightParen), Some(ParserToken::ObjectStart))
            | (Some(ParserToken::ArrayEnd), Some(ParserToken::Constant(_)))
            | (Some(ParserToken::ArrayEnd), Some(ParserToken::Variable(_)))
            | (Some(ParserToken::ArrayEnd), Some(ParserToken::ObjectStart))
            | (Some(ParserToken::Constant(_)), Some(ParserToken::ObjectStart))
            | (Some(ParserToken::Constant(_)), Some(ParserToken::Constant(_)))
            | (Some(ParserToken::Constant(_)), Some(ParserToken::Variable(_)))
            | (Some(ParserToken::Variable(_)), Some(ParserToken::ObjectStart))
            | (Some(ParserToken::Variable(_)), Some(ParserToken::Variable(_)))
            | (Some(ParserToken::Variable(_)), Some(ParserToken::Constant(_)))
            | (Some(ParserToken::Operator(_)), Some(ParserToken::Operator(_)))
            | (Some(ParserToken::ObjectEnd), Some(ParserToken::ObjectStart))
            | (Some(ParserToken::ObjectEnd), Some(ParserToken::Constant(_)))
            | (Some(ParserToken::ObjectEnd), Some(ParserToken::Variable(_))) => {
                return ParserResult::End(0);
            }
            _ => {}
        }

        self.last = token.clone();
        return match token {
            Some(t) => ParserResult::Some(t, self.size),
            None => ParserResult::End(0)
        }
    }
}
