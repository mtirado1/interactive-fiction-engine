use regex::Regex;
use crate::parser::{Parser, ParserResult};
use crate::expression::{Expression, Operator, UnaryOperator, ExpressionToken};
use crate::value::{Value, Value::*};
use lazy_static::lazy_static;

#[derive(Clone, PartialEq)]
pub enum ParserToken {
    LeftParen,   RightParen,
    FunctionStart, FunctionEnd,
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

use ParserToken::*;

impl ParserToken {
    fn is_start_token(&self) -> bool {
        match self {
            LeftParen | FunctionStart | ArrayStart | ObjectStart => true,
            _ => false
        }
    }

    fn is_end_token(&self) -> bool {
        match self {
            RightParen | FunctionEnd | ArrayEnd | ObjectEnd => true,
            _ => false
        }
    }

    fn is_value(&self) -> bool {
        match self {
            Constant(_) | Variable(_) => true,
            _ => false
        }
    }

    fn is_operator(&self) -> bool {
        match self {
            Operator(_) | UnaryOperator(_) => true,
            _ => false
        }
    }

    fn to_expression_operator(self) -> Option<ExpressionToken> {
        match self {
            Operator(op) => Some(ExpressionToken::Operator(op)),
            UnaryOperator(op) => Some(ExpressionToken::UnaryOperator(op)),
            _ => None
        }
    }

    fn invalid_pair(previous: Option<&Self>, token: Option<&Self>) -> bool {
        match (previous, token) {
            (None, Some(Operator(_))) => return true,
            (Some(a), Some(Operator(_))) => {
                return a.is_operator();
            },
            (Some(ObjectIndex(_)), Some(a)) => {
                return a.is_start_token() || a.is_value();
            }
            (Some(IndexStart), Some(a)) => {
                return a.is_end_token();
            }
            (Some(IndexEnd), Some(a)) => {
                return a.is_start_token() || a.is_value();
            }
            (Some(a), Some(b)) => {
                return (a.is_end_token() && b.is_start_token())
                || (a.is_value() && b.is_start_token())
                || (a.is_end_token() && b.is_value());
            }
            _ => return false
        }
    }
}

enum ListType {
    Function(String),
    Array,
    Object
}

pub struct ExpressionParser {
    last: Option<ParserToken>,
    token_stack: Vec<ParserToken>,
    size: usize
}

impl ExpressionParser {
    fn get_token(&mut self, regex: &Regex, search: &str) -> Option<String> {
        let capture = regex.captures(search)?;
        // Consume the entire regex match or only the first capture group.
        let token_string = capture.get(1).unwrap_or(capture.get(0).unwrap()).as_str();
        self.size += token_string.len();
        return Some(token_string.to_string());
    }

    pub fn new() -> Self {
        return Self { last: None, token_stack: Vec::new(), size: 0};
    }

    pub fn parse_indices(tokens: Vec<ParserToken>) -> Option<(String, Vec<Expression>)> {
        let variable_name: String;
        let mut token_stack = Vec::<ParserToken>::new();
        let mut expression_stack = Vec::<Expression>::new();

        let mut bracket_counter: usize = 0;
        if let Some(Variable(name)) = tokens.first() {
            variable_name = name.to_string();
        } else {
            return None
        }

        for token in tokens[1..].to_vec() {
            match (&token, bracket_counter) {
                (IndexStart, 0) => bracket_counter += 1,
                (IndexStart, _) => {
                    bracket_counter += 1;
                    token_stack.push(token);
                }
                (IndexEnd, 1) => {
                    if let Ok(expression) = Self::convert_to_postfix(token_stack) {
                        expression_stack.push(expression);
                    }
                    token_stack = Vec::<ParserToken>::new();
                }
                (IndexEnd, _) => {
                    token_stack.push(token);
                    bracket_counter -= 1;
                }
                (ObjectIndex(key), 0) => {
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
        while operator_stack.last().map_or(false, |token| condition(token) && token.is_operator()) {
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
                LeftParen | FunctionStart | IndexStart | UnaryOperator(_) => {
                    operator_stack.push(token);
                }
                RightParen => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &LeftParen);
                    if let None = operator_stack.pop() {
                        return Err(ParsingError::MismatchedParentheses);
                    }
                }
                FunctionEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &FunctionStart);
                    if let None = operator_stack.pop() {
                        return Err(ParsingError::MismatchedParentheses);
                    }
                    if let Some((mut arg_count, ListType::Function(name))) = function_stack.pop() {
                        if let Some(FunctionStart) = previous_token {
                            arg_count = 0;
                        }
                        return_expression.push(ExpressionToken::Function(name, arg_count));
                    }
                }
                ObjectSeparator => {
                }
                Separator => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |_| true);
                    if let Some((arg_count, list_type)) = function_stack.pop() {
                        function_stack.push((arg_count + 1, list_type));
                    }
                    else {
                        return Err(ParsingError::InvalidSeparatorToken);
                    }
                }
                ArrayStart => {
                    operator_stack.push(token);
                    function_stack.push((1, ListType::Array));
                }
                ObjectStart => {
                    operator_stack.push(token);
                    function_stack.push((1, ListType::Object));
                }
                ObjectEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &ObjectStart);
                    if let Some(ObjectStart) = operator_stack.pop() {
                        if let Some((mut arg_count, ListType::Object)) = function_stack.pop() {
                            if let Some(ObjectStart) = previous_token {
                                arg_count = 0;
                            }
                            return_expression.push(ExpressionToken::Object(arg_count));
                        }
                    }
                }
                ArrayEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &ArrayStart);
                    if let Some(ArrayStart) = operator_stack.pop() {
                        if let Some((mut arg_count, ListType::Array)) = function_stack.pop() {
                            if let Some(ArrayStart) = previous_token {
                                arg_count = 0;
                            }
                            return_expression.push(ExpressionToken::Array(arg_count));
                        }
                        else {
                            return Err(ParsingError::MismatchedBrackets);
                        }
                    }
                }
                IndexEnd => {
                    Self::pop_while(&mut operator_stack, &mut return_expression, |x| x != &IndexStart);
                    if let Some(IndexStart) = operator_stack.pop() {
                        return_expression.push(ExpressionToken::Operator(Operator::Index));
                    }
                    else {
                        return Err(ParsingError::MismatchedBrackets);
                    }
                }
                Function(name) => {
                    let list = (1, ListType::Function(name));
                    function_stack.push(list);
                }
                Operator(ref op) => {
                    let precedence = op.precedence();
                    Self::pop_while(&mut operator_stack, &mut return_expression, |last| -> bool {
                        let last_precedence = match last {
                            Operator(op) => op.precedence(),
                            UnaryOperator(op) => op.precedence(),
                            _ => return false
                        };
                        return (last_precedence > precedence) || ((last_precedence == precedence) && op.is_left_associative());
                    });
                    operator_stack.push(token);
                }
                ObjectIndex(index) => {
                    return_expression.push(ExpressionToken::Constant(Text(index)));
                    return_expression.push(ExpressionToken::Operator(Operator::Index));
                }
                Variable(var) => {
                    return_expression.push(ExpressionToken::Variable(var.to_string()));
                }
                Constant(value) => {
                    return_expression.push(ExpressionToken::Constant(value.clone()));
                }
            }
            previous_token = Some(token_reference.clone());
        }
        Self::pop_while(&mut operator_stack, &mut return_expression, |_| true);
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
            let start = match self.last {
                Some(Function(_)) => FunctionStart,
                _ => LeftParen
            };
            self.token_stack.push(start.clone());
            token = Some(start);
        }
        else if let Some(_) = self.get_token(&RIGHT_PAREN_REGEX, slice) {
            token = match self.token_stack.pop() {
                Some(LeftParen) => Some(RightParen),
                Some(FunctionStart) => Some(FunctionEnd),
                _ => None
            }
        }
        else if let Some(_) = self.get_token(&ARRAY_START_REGEX, slice) {
            let start = match self.last {
                Some(RightParen) | Some(ArrayEnd) |
                Some(ObjectEnd) |Some(IndexEnd) |
                Some(ObjectIndex(_)) |
                Some(Constant(_)) |
                Some(Variable(_)) => IndexStart,
                _ => ArrayStart
            };
            self.token_stack.push(start.clone());
            token = Some(start);
        }
        else if let Some(_) = self.get_token(&ARRAY_END_REGEX, slice) {
            token = match self.token_stack.pop() {
                Some(ArrayStart) => Some(ArrayEnd),
                Some(IndexStart) => Some(IndexEnd),
                _ => None
            };
        }
        else if let Some(_) = self.get_token(&OBJECT_START_REGEX, slice) {
            token = Some(ObjectStart);
            self.token_stack.push(ObjectStart);
        }
        else if let Some(_) = self.get_token(&OBJECT_END_REGEX, slice) {
            if let Some(ObjectStart) = self.token_stack.pop() {
                token = Some(ObjectEnd);
            }
        }
        else if let Some(_) = self.get_token(&SEPARATOR_REGEX, slice) {
            token = Some(Separator);
        }
        else if let Some(_) = self.get_token(&OBJECT_SEPARATOR_REGEX, slice) {
            if let Some(ObjectStart) = self.token_stack.last() {
                token = Some(ObjectSeparator);
            }
        }
        else if let Some(s) = self.get_token(&BOOLEAN_REGEX, slice) {
            token = Some(Constant(Boolean(s == "true")));
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
            token = Some(Constant(Text(new_string)));
        }
        else if let Some(s) = self.get_token(&FLOAT_REGEX, slice) {
            let float = s.parse::<f64>().unwrap();
            token = Some(Constant(Float(float)));
        }
        else if let Some(s) = self.get_token(&INTEGER_REGEX, slice) {
            let integer = s.parse::<i64>().unwrap();
            token = Some(Constant(Integer(integer)));
        }
        else if let Some(_) = self.get_token(&NULL_REGEX, slice) {
            token = Some(Constant(Null));
        }
        else if let Some(_) = self.get_token(&CONTAINS_REGEX, slice) {
            token = Some(Operator(Operator::Contains));
        }
        else if let Some(_) = self.get_token(&IN_REGEX, slice) {
            token = Some(Operator(Operator::In));
        }
        else if let Some(_) = self.get_token(&AND_REGEX, slice) {
            token = Some(Operator(Operator::And));
        }
        else if let Some(_) = self.get_token(&OR_REGEX, slice) {
            token = Some(Operator(Operator::Or));
        }
        else if let Some(_) = self.get_token(&NOT_REGEX, slice) {
            token = Some(UnaryOperator(UnaryOperator::Not));
        }
        else if let Some(name) = self.get_token(&FUNCTION_REGEX, slice) {
            token = Some(Function(name));
        }
        else if let Some(var) = self.get_token(&VARIABLE_REGEX, slice) {
            token = Some(Variable(var));
        }
        else if let Some(_) = self.get_token(&EQ_REGEX, slice) {
            token = Some(Operator(Operator::Equal));
        }
        else if let Some(_) = self.get_token(&NEQ_REGEX, slice) {
            token = Some(Operator(Operator::NotEqual));
        }
        else if let Some(_) = self.get_token(&GTE_REGEX, slice) {
            token = Some(Operator(Operator::GreaterOrEqual));
        }        
        else if let Some(_) = self.get_token(&GT_REGEX, slice) {
            token = Some(Operator(Operator::Greater));
        }
        else if let Some(_) = self.get_token(&LTE_REGEX, slice) {
            token = Some(Operator(Operator::LessOrEqual));
        }
        else if let Some(_) = self.get_token(&LT_REGEX, slice) {
            token = Some(Operator(Operator::Less));
        }
        else if let Some(_) = self.get_token(&POW_REGEX, slice) {
            token = Some(Operator(Operator::Exp));
        }
        else if let Some(s) = self.get_token(&INDEX_REGEX, slice) {
            let mut chars = s.chars();
            chars.next();
            let index = chars.as_str().to_string();
            token = Some(ObjectIndex(index));
        }
        else if let Some(_) = self.get_token(&MOD_REGEX, slice) {
            token = Some(Operator(Operator::Rem));
        }
        else if let Some(_) = self.get_token(&MUL_REGEX, slice) {
            token = Some(Operator(Operator::Mul));
        }
        else if let Some(_) = self.get_token(&DIV_REGEX, slice) {
            token = Some(Operator(Operator::Div));
        }
        else if let Some(_) = self.get_token(&PLUS_REGEX, slice) {
            token = match self.last {
                None |
                Some(LeftParen) |
                Some(ArrayStart) |
                Some(Operator(_)) |
                Some(UnaryOperator(_)) => Some(UnaryOperator(UnaryOperator::Plus)),
                _ => Some(Operator(Operator::Add))
            };
        }
        else if let Some(_) = self.get_token(&MINUS_REGEX, slice) {
            token = match self.last {
                None | 
                Some(LeftParen) |
                Some(ArrayStart) |
                Some(Operator(_)) |
                Some(UnaryOperator(_)) => Some(UnaryOperator(UnaryOperator::Minus)),
                _ => Some(Operator(Operator::Sub))
            };
        }
        else if let Some(_) = self.get_token(&COALESCE_REGEX, slice) {
            token = Some(Operator(Operator::Coalesce));
        }
        else if let Some(_) = self.get_token(&END_REGEX, slice) {
            return ParserResult::End(self.size);
        }

        if ParserToken::invalid_pair(self.last.as_ref(), token.as_ref()) {
            return ParserResult::End(0);
        }

        self.last = token.clone();
        return match token {
            Some(t) => ParserResult::Some(t, self.size),
            None => ParserResult::End(0)
        }
    }
}
