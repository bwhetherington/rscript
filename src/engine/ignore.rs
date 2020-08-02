use crate::engine::Str;
use std::collections::HashSet;

type Frame = HashSet<Str>;

#[derive(Debug)]
pub struct Ignore {
    frames: Vec<Frame>,
}

impl Ignore {
    pub fn new() -> Ignore {
        Ignore { frames: Vec::new() }
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
    pub fn contains(&self, key: impl AsRef<str>) -> bool {
        let key = key.as_ref();
        self.frames().any(|frame| frame.contains(key))
    }

    pub fn insert(&mut self, key: impl Into<Str>) {
        let key = key.into();
        match self.frames.last_mut() {
            Some(frame) => {
                frame.insert(key);
            }
            None => {
                let mut frame = HashSet::new();
                frame.insert(key);
                self.frames.push(frame);
            }
        }
    }

    pub fn descend(&mut self) {
        self.frames.push(HashSet::new());
    }

    pub fn ascend(&mut self) -> Option<Frame> {
        self.frames.pop()
    }
}
