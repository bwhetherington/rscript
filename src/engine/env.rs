use crate::engine::{vm2::Value, Str};
use std::collections::HashMap;

pub type Frame = HashMap<Str, Value>;

#[derive(Debug)]
pub struct Environment {
    frames: Vec<Frame>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment { frames: Vec::new() }
    }

    /// Produces an iterator over the stack frames of the environment, yielding
    /// references to each frame.
    pub fn frames(&self) -> impl Iterator<Item = &Frame> {
        self.frames.iter().rev()
    }

    /// Produces an iterator over the stack frames of the environment, yielding
    /// mutable references to each frame.
    pub fn frames_mut(&mut self) -> impl Iterator<Item = &mut Frame> {
        self.frames.iter_mut().rev()
    }

    /// Searches for the specified key in all stack frames in descending order,
    /// producing a reference to its topmost occurrence if present.
    pub fn get(&self, key: impl AsRef<str>) -> Option<&Value> {
        let key = key.as_ref();
        for frame in self.frames() {
            match frame.get(key) {
                Some(val) => return Some(val),
                None => (),
            }
        }
        None
    }

    /// Searches for the specified key in all stack frames in descending order,
    /// producing a mutable reference to its topmost occurrence if present.
    pub fn get_mut(&mut self, key: impl AsRef<str>) -> Option<&mut Value> {
        let key = key.as_ref();
        for frame in self.frames_mut() {
            match frame.get_mut(key) {
                Some(val) => return Some(val),
                None => (),
            }
        }
        None
    }

    pub fn insert(&mut self, key: impl Into<Str>, val: Value) {
        let key = key.into();
        match self.frames.last_mut() {
            Some(frame) => {
                frame.insert(key, val);
            }
            None => {
                let mut frame = HashMap::new();
                frame.insert(key, val);
                self.frames.push(frame);
            }
        }
    }

    pub fn descend(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn ascend(&mut self) -> Option<Frame> {
        self.frames.pop()
    }
}
