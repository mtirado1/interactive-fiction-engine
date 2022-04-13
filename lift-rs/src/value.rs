use std::fmt;
use std::ops;
use std::collections::HashMap;
use fastrand;

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Text(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Boolean(bool),
    Null
}

use Value::*;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
        Integer(a) => a.to_string(),
        Float(a) => a.to_string(),
        Text(a) => a.to_string(),
        Boolean(b) => {if *b { "true" } else { "false" }}.to_string(),
        Array(items) => {
            format!("[{}]", items.iter()
                    .map(|item| {item.to_string()})
                    .collect::<Vec<String>>()
                    .join(", "))
        },
        Object(obj) => {
            format!("{{{}}}", obj.iter()
                    .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                    .collect::<Vec<String>>()
                    .join(", "))
        }
        Null => "null".to_string()
        };
        write!(f, "{}", r)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Integer(a), Integer(b)) => a == b,
            (Float(a), Integer(b)) => *a == *b as f64,
            (Integer(a), Float(b)) => *a as f64 == *b,
            (Float(a), Float(b)) => a == b,
            (Boolean(a), Boolean(b)) => a == b,
            (Text(a), Text(b)) => a == b,
            (Null, Null) => true,
            (Array(a), Array(b)) => {
                (a.len() == b.len()) && a.iter().zip(b).all(|(i, j)| { i == j })
            },
            _ => false
        }
    }
}   

impl Value {
    pub fn is_true(&self) -> bool {
        match self {
            Integer(a) => { *a != 0 },
            Float(a) => { *a != 0.0 },
            Text(a) => { *a != "" },
            Boolean(a) => { *a },
            Array(a) => { a.len() > 0 }
            _ => false
        }
    }

    pub fn coalesce(&self, other: &Value) -> Value {
        match self {
            Null => other.clone(),
            _ => self.clone()
        }
    }

    pub fn pow(&self, other: &Value) -> Value {
        match (self, other) {
            (Integer(a), Integer(b)) => Float((*a as f64).powf(*b as f64)),
            (Integer(a), Float(b)) => Float((*a as f64).powf(*b)),
            (Float(a), Integer(b)) => Float(a.powf(*b as f64)),
            (Float(a), Float(b)) => Float(a.powf(*b)),
            _ => Null
        }
    }

    pub fn get_mut(&mut self, references: &Vec<Value>) -> Option<&mut Value> {
        let mut reference: &mut Value = self;

        for index in references.iter() {
            match (&mut *reference, index) {
                (Array(arr), Integer(i)) => {
                    reference = arr.get_mut(*i as usize)?;
                }
                (Object(obj), Text(key)) => {
                    if obj.contains_key(key) {
                        reference = obj.get_mut(key).unwrap();
                    }
                    else {
                        obj.insert(key.to_string(), Null);
                        reference = obj.get_mut(key).unwrap();
                    }
                }
                _ => return None
            }
        }
        return Some(reference);
    }

    pub fn iter(&self) -> Vec<(Value, Value)> {
        match self {
            Array(arr) => arr.iter().enumerate().map(|(index, value)| (Integer(index as i64), value.clone())).collect(),
            Text(s) => s.chars().enumerate().map(|(index, value)| (Integer(index as i64), Text(value.to_string()))).collect(),
            Object(o) => o.iter().map(|(key, value)| (Value::Text(key.to_string()), value.clone())).collect(),
            _ => vec![]
        }
    }
}


impl ops::Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Integer(a) => Integer(-a),
            Float(a) => Float(-a),
            _ => Null
        }
    }
}

impl ops::Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a + b),
            (Float(a),   Integer(b)) => Float(a + b as f64),
            (Integer(a), Float(b))   => Float(a as f64 + b),
            (Float(a),   Float(b))   => Float(a + b),
            (Text(a),    Text(b)) => {
                let mut result = a.clone();
                result.push_str(&b);
                Text(result)
            },
            (Array(a), Array(b)) => {
                let mut c = a.clone();
                c.extend(b.clone());
                Array(c)
            },
            _ => Value::Null
        }
    }
}

impl ops::Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a * b),
            (Integer(a), Float(b))   => Float(a as f64 * b),
            (Float(a), Integer(b))   => Float(a * b as f64),
            (Float(a), Float(b))     => Float(a * b),
            (Array(a), Integer(b))   => {
                let mut ret = Vec::<Value>::new();
                for _ in 0..b {
                    ret.extend(a.clone());
                }
                Array(ret)
            }
            (Integer(b), Array(a)) => {
                let mut ret = Vec::<Value>::new();
                for _ in 0..b {
                    ret.extend(a.clone());
                }
                Array(ret)
            }
            (Text(a), Integer(b))    => Text(a.repeat(b as usize)),
            (Integer(a), Text(b))    => Text(b.repeat(a as usize)),
            _ => Null
        }
    }
}


impl ops::Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Integer(a), Integer(b)) => Float(a as f64 / b as f64),
            (Integer(a), Float(b))   => Float(a as f64 / b),
            (Float(a), Integer(b))   => Float(a / b as f64),
            (Float(a), Float(b))     => Float(a / b),
            _ => Null
        }
    }
}

impl ops::Rem for Value {
    type Output = Value;
    
    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a % b),
            (Float(a), Integer(b)) => Float(a % b as f64),
            (Integer(a), Float(b)) => Float(a as f64 % b),
            (Float(a), Float(b)) => Float(a % b),
            _ => Null
        }
    }
}

impl ops::Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Integer(a), Integer(b)) => Integer(a - b),
            (Integer(a), Float(b))   => Float(a as f64 - b),
            (Float(a), Integer(b))   => Float(a - b as f64),
            (Float(a), Float(b))     => Float(a - b),
            _ => Null
        }
    }
}


pub mod comparison {
    use crate::value::{Value, Value::*};
    fn numeric_comparison<F>(lhs: &Value, rhs: &Value, lambda: F) -> Value where F: Fn(f64, f64) -> bool {
        match (lhs, rhs) {
            (Integer(a), Integer(b)) => Boolean(lambda(*a as f64, *b as f64)),
            (Float(a),   Integer(b)) => Boolean(lambda(*a, *b as f64)),
            (Integer(a),   Float(b)) => Boolean(lambda(*a as f64, *b)),
            (Float(a),     Float(b)) => Boolean(lambda(*a, *b)),
            (Text(a),       Text(b)) => Boolean(lambda(a.len() as f64, b.len() as f64)),
            (Array(a),     Array(b)) => Boolean(lambda(a.len() as f64, b.len() as f64)),
            _ => Null
        }
    }

    pub fn gt(lhs: &Value, rhs: &Value) -> Value {
        numeric_comparison(lhs, rhs, |a, b| {a > b})
    }

    pub fn gte(lhs: &Value, rhs: &Value) -> Value {
        numeric_comparison(lhs, rhs, |a, b| {a >= b})
    }

    pub fn lt(lhs: &Value, rhs: &Value) -> Value {
        numeric_comparison(lhs, rhs, |a, b| {a < b})
    }

    pub fn lte(lhs: &Value, rhs: &Value) -> Value {
        numeric_comparison(lhs, rhs, |a, b| {a <= b})
    }
}


pub mod operator {
    use crate::value::{Value, Value::*};
    pub fn contains(lhs: &Value, rhs: &Value) -> Value {
        match (lhs, rhs) {
            (Array(a), b) => Boolean(a.contains(b)),
            (Object(a), Text(b)) => Boolean(a.contains_key(b)),
            (Text(a), Text(b)) => Boolean(a.contains(b)),
            _ => Null
        }
    }

    pub fn index(lhs: &Value, rhs: &Value) -> Value {
        match(lhs, rhs) {
            (Object(a), Text(index)) => a.get(index).unwrap_or(&Null).clone(),
            (Array(a), Integer(index)) => a.get(*index as usize).unwrap_or(&Null).clone(),
            (Text(s), Integer(index)) => s.chars().nth(*index as usize).map_or(Null, |c| Text(c.to_string())),
            _ => Null            
        }
    }
}

impl Value {
    pub fn eval_function(name: &str, values: Vec<Value>) -> Value {
        match name {
            "int" => {
                match values.get(0) {
                    Some(Text(s)) => s.parse::<i64>().map_or(Null, |i| Integer(i)),
                    Some(Integer(i)) => Integer(*i),
                    Some(Float(f)) => Integer(*f as i64),
                    _ => Null
                }
            }
            "float" => {
                match values.get(0) {
                    Some(Text(s)) => s.parse::<f64>().map_or(Null, |i| Float(i)),
                    Some(Integer(i)) => Float(*i as f64),
                    Some(Float(f)) => Float(*f),
                    _ => Null
                }
            }
            "str" | "string" => Text(values.get(0).map_or(String::new(), |v| v.to_string())),
            "len" => {
                match values.get(0) {
                    Some(Array(a)) => Integer(a.len() as i64),
                    Some(Object(o)) => Integer(o.len() as i64),
                    Some(Text(s)) => Integer(s.len() as i64),
                    _ => Null
                }
            }
            "keys" => {
                match values.get(0) {
                    Some(Object(o)) => Array(o.keys().map(|k| Text(k.to_string())).collect()),
                    _ => Null
                }
            }
            "pickrandom" => {
                match values.get(0) {
                    Some(Array(vec)) => {
                        if vec.len() > 0 {
                            let i = fastrand::usize(..vec.len());
                            vec[i].clone()
                        }
                        else { Null }
                    }
                    _ => Null
                }
            }
            "probability" => {
                match values.get(0) {
                    Some(Float(p)) => { Boolean(fastrand::f64() < *p) },
                    Some(Integer(p)) => { Boolean(fastrand::f64() < *p as f64) },
                    _ => Null
                }
            }
            "rand" => {
                match (values.get(0), values.get(1)) {
                    (None, None) => Float(fastrand::f64()),
                    (Some(Integer(a)), Some(Integer(b))) => Integer(fastrand::i64(a..b)),
                    (Some(Float(a)), Some(Float(b))) => Float(a + fastrand::f64() * (b - a)),
                    (Some(Integer(a)), Some(Float(b))) => Float(*a as f64 + fastrand::f64() * (b - *a as f64)),
                    (Some(Float(a)), Some(Integer(b))) => Float(a + fastrand::f64() * (*b as f64 - a)),
                    _ => Null
                }
            }
            "range" => {
                match (values.get(0), values.get(1)) {
                    (Some(Integer(start)), None) => {
                        Array((0..*start).map(|x| -> Value {Integer(x)}).collect())
                    }
                    (Some(Integer(start)), Some(Integer(end))) => {
                        Array((*start..*end).map(|x| -> Value {Integer(x)}).collect())
                    }
                    _ => Null
                }
            }
            "sqrt" => {
                match values.get(0) {
                    Some(Integer(k)) => Float((*k as f64).sqrt()),
                    Some(Float(k)) => Float(k.sqrt()),
                    _ => Null
                }
            }
            "abs" => {
                match values.get(0) {
                    Some(Integer(k)) => Integer(k.abs()),
                    Some(Float(k)) => Float(k.abs()),
                    _ => Null
                }
            }
            "if" => {
                let mut return_value = Null;
                let mut iterator = values.iter();
                while let Some(value) = iterator.next() {
                    let next = iterator.next();
                    if value.is_true() && next.is_some() {
                        return_value = next.unwrap_or(value).clone();
                        break;
                    }
                    else {
                        return_value = value.clone();
                    }
                }
                return_value
            }
            _ => Null
        }
    }
}
