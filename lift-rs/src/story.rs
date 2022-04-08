// Lift Interpreter Core
use std::collections::HashMap;
use std::rc::Rc;
use regex::Regex;

use crate::content::{Page, Content, Action};
use crate::expression::StateManager;
use crate::value::Value;

#[derive(Clone)]
pub enum Element {
    Text(String),
    Link(String, String),
    ContentLink(String, String, usize),
    JumpLink(String, String, String, usize),
    Error(String)
}

enum StoryAction {
    Goto(String),
    Halt
}

struct StoryResult {
    output: Vec<Element>,
    action: StoryAction
}

impl StoryResult {
    fn new() -> Self {
        StoryResult { output: Vec::<Element>::new(), action: StoryAction::Halt }
    }

    fn combine(&mut self, mut result: StoryResult) {
        self.output.append(&mut result.output);
        self.action = result.action;
    }

    fn push_element(&mut self, element: Element) {
        self.output.push(element);
    }
}



pub struct Story {
    first_page: String,
    pages: HashMap<String, Page>
}

impl Story {
    pub fn new(source: &str) -> Self {
        let header_regex = Regex::new(r"^#+ (?P<title>.+)").unwrap();

        let mut pages = HashMap::<String, Page>::new();
        let mut content_acumulator = "".to_string();
        let mut first_page: Option<&str> = None;
        let mut current_page: Option<&str> = None;

        for line in source.lines() {
            let cap = header_regex.captures(line);

            if let Some(capture) = cap {
                if let Some(title) = current_page {
                    let page = Page::parse(title, &content_acumulator);
                    pages.insert(title.to_string(), page);
                    content_acumulator = "".to_string();
                }
                let title = capture.name("title").unwrap().as_str().trim();
                if first_page == None {
                    first_page = current_page
                }
                current_page = Some(title);
            } else if let Some(_) = current_page {
                content_acumulator += &format!("{}\n", line);
            }
        }
        if let Some(title) = current_page {
            let page = Page::parse(title, &content_acumulator);
            pages.insert(title.to_string(), page);
            if first_page == None {
                first_page = Some(title)
            }
        }
        Story {pages, first_page: first_page.unwrap_or("").to_string()}
    }

    fn get_page(&self, title: &str) -> Option<&Page> {
        self.pages.get(title)
    }

    fn get_action(&self, title: &str, index: usize) -> Option<&Vec<Content>> {
        if let Some(page) = self.get_page(title) {
            if let Some(action) = page.actions.get(index) {
                return Some(action)
            }
        }
        return None;
    }
}

struct State {
    current_page: String,
    global: HashMap<String, Value>,
    local: HashMap<String, HashMap<String, Value>>
}

impl State {
    fn new(story: &Story) -> State {
        let mut local = HashMap::<String, HashMap<String, Value>>::new();
        for (title, _) in story.pages.iter() {
            let mut s = HashMap::<String, Value>::new();
            s.insert("visited".to_string(), Value::Integer(0));
            local.insert(title.to_string(), s);
        }
        State {
            current_page: story.first_page.clone(),
            global: HashMap::<String, Value>::new(),
            local
        }
    }

    fn get(&self, page: &str, variable: &str) -> Option<&Value> {
        if let Some(state) = self.local.get(page) {
            if let Some(value) = state.get(variable) {
                return Some(value);
            }
        }
        return self.global.get(variable);
    }

    fn set(&mut self, variable: &str, value: Value) {
        self.global.insert(variable.to_string(), value);
    }

    fn set_index(&mut self, variable: &str, indices: &Vec<Value>, value: Value) {
        if indices.is_empty() {
            return self.set(variable, value);
        }
        if let Some(var) = self.global.get_mut(variable) {
            if let Some(reference) = var.get_mut(indices) {
                *reference = value;
            }
        }
    }

    fn set_local(&mut self, variable: &str, value: Value) {
        if let Some(state) = self.local.get_mut(&self.current_page) {
            state.insert(variable.to_string(), value);
        }
    }

    fn set_local_index(&mut self, variable: &str, indices: &Vec<Value>, value: Value) {
        if indices.is_empty() {
            return self.set_local(variable, value);
        }
        if let Some(state) = self.local.get_mut(&self.current_page) {
            if let Some(var) = state.get_mut(variable) {
                if let Some(reference) = var.get_mut(indices) {
                    *reference = value
                }
            }
        }
    }
}

impl StateManager for State {
    fn get(&self, variable: &str) -> Option<&Value> {
        self.get(&self.current_page, variable)
    }
}

pub struct Interpreter {
    story: Rc<Story>,
    output: Vec<Element>,
    state: State
}

impl Interpreter {
    pub fn new(story: Story) ->  Self {
        let state = State::new(&story);
        Interpreter {
            story: Rc::new(story),
            state,
            output: Vec::<Element>::new()
        }
    }

    pub fn send(&mut self, index: usize) {
        let element: Option<Element> = self.output.get(index).cloned();
        if let Some(Element::Link(_, destination)) = element {
            self.state.current_page = destination.to_string();
            self.play();
        }
        else if let Some(Element::ContentLink(_, page, action_index)) = element {
            if let Some(content) = self.story.clone().get_action(&page, action_index) {
                let result = self.eval(content);
                match result.action {
                    StoryAction::Halt => {
                        self.output.splice(index..index+1, result.output);
                    }
                    StoryAction::Goto(p) => {
                        self.state.current_page = p;
                        self.play();
                        self.output.splice(0..0, result.output);
                    }
                }
            }
        }
        else if let Some(Element::JumpLink(_, destination, page, action_index)) = element {
            if let Some(content) = self.story.clone().get_action(&page, action_index) {
                let result = self.eval(content);
                self.state.current_page = destination.to_string();
                self.play();
                self.output.splice(0..0, result.output);
            }
        }
    }

    pub fn play(&mut self) {
        self.output.clear();
        loop {
            if let Some(page) = self.story.clone().get_page(&self.state.current_page) {
                let mut result = self.eval(&page.content);
                self.output.append(&mut result.output);
                match result.action {
                    StoryAction::Halt => break,
                    StoryAction::Goto(p) => {
                        self.output.clear();
                        self.state.current_page = p
                    }
                }
            }
            else {
                self.output.push(Element::Error(format!("Invalid page: {}", self.state.current_page)));
                break;
            }
        }
    }

    pub fn output(&self) -> &Vec<Element> {
        &self.output
    }

    fn eval(&mut self, content: &Vec<Content>) -> StoryResult {
        let mut result = StoryResult::new();
        let mut if_action: Option<bool> = None;
        for element in content.iter() {
            match element {
                Content::Text(s) => result.push_element(Element::Text(s.eval(&self.state))),
                Content::Link(link) => {
                    let element = match link {
                        Action::Normal{title, destination} => {
                            Element::Link(title.eval(&self.state), destination.eval(&self.state))
                        }
                        Action::Content{title, page, action} => {
                            Element::ContentLink(title.eval(&self.state), page.to_string(), *action) 
                        }
                        Action::JumpLink{title, destination, page, action} => {
                            Element::JumpLink(title.eval(&self.state), destination.eval(&self.state), page.to_string(), *action) 
                        }
                    };
                    result.push_element(element);
                }
                Content::Goto(page) => {result.action = StoryAction::Goto(page.eval(&self.state))},
                Content::Import(page_title) => {
                    if let Some(page) = self.story.clone().get_page(&page_title.eval(&self.state)) {
                        let import_result = self.eval(&page.content);
                        result.combine(import_result);
                    }
                }
                Content::Set{local, variable, indices, expression} => {
                    let value = expression.eval(&self.state);
                    let ind: Vec<_> = indices.iter().map(|x| x.eval(&self.state)).collect();
                    if *local {
                        self.state.set_local_index(variable, &ind, value);
                    }
                    else {
                        self.state.set_index(variable, &ind, value);
                    }
                }
                Content::If{expression, content} => {
                    if_action = Some(expression.eval(&self.state).is_true());
                    if let Some(true) = if_action {
                        let content_result = self.eval(content);
                        result.combine(content_result);
                    }
                }
                Content::ElseIf{expression, content} => {
                    if let Some(false) = if_action {
                        if_action = Some(expression.eval(&self.state).is_true());
                        if let Some(true) = if_action {
                            let content_result = self.eval(content);
                            result.combine(content_result);
                        }
                    }
                }
                Content::Else { content } => {
                    if let Some(false) = if_action {
                        if_action = None;
                        let content_result = self.eval(content);
                        result.combine(content_result);
                    }
                }
                Content::For { index, variable, expression, content} => {
                    let iterator_value = expression.eval(&self.state);
                    if let Value::Array(arr) = iterator_value {
                        for (i, value) in arr.iter().enumerate() {
                            if let Some(index) = index {
                                self.state.set_local(index, Value::Integer(i as i64));
                            }
                            self.state.set_local(variable, value.clone());
                            let content_result = self.eval(content);
                            result.combine(content_result);
                            if let StoryAction::Goto(_) = result.action {
                                break;
                            }
                        }
                    } else if let Value::Object(obj) = iterator_value {
                        for (key, value) in obj {
                            if let Some(index) = index {
                                self.state.set_local(index, Value::Text(key));
                            }
                            self.state.set_local(variable, value);
                            let content_result = self.eval(content);
                            result.combine(content_result);
                            if let StoryAction::Goto(_) = result.action {
                                break;
                            }
                        }
                    }
                }
                Content::While {expression, content} => {
                    while expression.eval(&self.state).is_true() {
                        let content_result = self.eval(content);
                        result.combine(content_result);
                        if let StoryAction::Goto(_) = result.action {
                            break;
                        }
                    }
                }
                Content::Error(e) => result.push_element(Element::Error(e.to_string()))
            }
            if let StoryAction::Goto(_) = result.action {
                return result;
            }
        }
        return result;
    }
}