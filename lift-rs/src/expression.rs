use std::collections::HashMap;
use crate::value::{Value, Value::*, comparison, operator};

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
    pub fn precedence(&self) -> u32 {
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

    pub fn is_left_associative(&self) -> bool {
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
    pub fn precedence(&self) -> u32 {
        match self {
            Minus | Plus | Not => 100
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
