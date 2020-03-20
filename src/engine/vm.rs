use crate::{engine::Node, parser::*};
use std::cell::RefCell;
use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    error::Error,
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

pub fn is_usize(num: f64) -> bool {
    (num as usize as f64) == num
}

#[derive(Debug)]
struct Capture {
    frames: Vec<HashSet<String>>,
}

macro_rules! is_type {
    ($val:expr, $orig:ty, $ty:ty) => {
        $val as $ty as $orig == $val
    };
}

impl Capture {
    pub fn new() -> Capture {
        Capture { frames: Vec::new() }
    }

    pub fn frames(&self) -> impl Iterator<Item = &HashSet<String>> {
        self.frames.iter().rev()
    }

    pub fn descend(&mut self) {
        self.frames.push(HashSet::new());
    }

    pub fn ascend(&mut self) -> Option<HashSet<String>> {
        self.frames.pop()
    }

    pub fn contains(&self, key: impl AsRef<str>) -> bool {
        let key = key.as_ref();
        for frame in self.frames() {
            if frame.get(key).is_some() {
                return true;
            }
        }
        false
    }

    pub fn insert(&mut self, value: impl Into<String>) {
        let value = value.into();
        match self.frames.last_mut() {
            Some(frame) => {
                frame.insert(value);
            }
            None => {
                let mut frame = HashSet::new();
                frame.insert(value);
                self.frames.push(frame);
            }
        }
    }
}

type Frame = HashMap<String, Value>;

#[derive(Debug)]
struct Environment {
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

    pub fn insert(&mut self, key: impl Into<String>, val: Value) {
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

pub struct Engine {
    instructions: Vec<Statement>,
    env: Environment,
    loop_depth: usize,
    path: Vec<String>,
    modules: HashMap<Vec<String>, Value>,
    interned_strings: HashMap<String, Value>,
    cur_module: String,
    pub root: Node,
}

#[derive(Debug)]
pub enum EvalError {
    KeyNotFoundError(String),
    TypeMismatchError(String, String),
    UndefinedError(String),
    InvalidStatement,
    ArityMismatchError(usize, usize),
    ParseError(ParseError),
}

impl From<ParseError> for EvalError {
    fn from(err: ParseError) -> EvalError {
        EvalError::ParseError(err)
    }
}

impl EvalError {
    #[inline]
    pub fn key_not_found(key: impl Into<String>) -> EvalError {
        EvalError::KeyNotFoundError(key.into())
    }

    #[inline]
    pub fn type_mismatch(expected: impl Into<String>, found: impl Into<String>) -> EvalError {
        EvalError::TypeMismatchError(expected.into(), found.into())
    }

    #[inline]
    pub fn undefined(ident: impl Into<String>) -> EvalError {
        EvalError::UndefinedError(ident.into())
    }

    #[inline]
    pub fn arity_mismatch(expected: usize, found: usize) -> EvalError {
        EvalError::ArityMismatchError(expected, found)
    }
}

fn matches_prefix<T: PartialEq + fmt::Debug>(
    prefix: impl Iterator<Item = T>,
    path: impl Iterator<Item = T>,
) -> bool {
    prefix.zip(path).all(|(a, b)| a == b)
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use EvalError::*;
        match self {
            ParseError(err) => write!(f, "{}", err),
            UndefinedError(ident) => write!(f, "identifier `{}` is undefined", ident),
            TypeMismatchError(expected, found) => {
                write!(f, "expected type `{}`, found `{}`", expected, found)
            }
            ArityMismatchError(expected, found) => {
                write!(f, "expected {} args, found {}", expected, found)
            }
            KeyNotFoundError(key) => write!(f, "key `{}` not found in object", key),
            InvalidStatement => write!(f, "invalid statement"),
        }
    }
}

impl Error for EvalError {}

type EvalResult<T> = Result<T, EvalError>;

fn hash_f64<H: Hasher>(f: f64, h: &mut H) {
    let int = match f {
        f if f.is_infinite() && f.is_sign_positive() => std::i32::MAX,
        f if f.is_infinite() => std::i32::MIN,
        f if f.is_nan() => 0,
        f => f as i32,
    };
    int.hash(h);
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Value::*;
        match self {
            Number(n) => hash_f64(*n, state),
            Boolean(b) => b.hash(state),
            String(s) => s.hash(state),
            List(vals) => vals.borrow().hash(state),
            Symbol(s) => s.hash(state),
            Object(obj) => obj.borrow().hash(state),
            None => 0.hash(state),
            Link(path) => path.hash(state),
            Function(_) => 0.hash(state),
        }
    }
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            instructions: Vec::new(),
            env: Environment::new(),
            loop_depth: 0,
            path: Vec::new(),
            modules: HashMap::new(),
            interned_strings: HashMap::new(),
            cur_module: "<None>".into(),
            root: Node::module("<root>"),
        }
    }

    pub fn cur_module(&self) -> &str {
        &self.cur_module
    }

    fn lookup_string(&mut self, s: impl AsRef<str>) -> EvalResult<Value> {
        let key = s.as_ref();
        let existing = self.interned_strings.get(key);
        existing.map(|x| Ok(x.clone())).unwrap_or_else(|| {
            let value = self.make_string(key)?;
            self.interned_strings.insert(key.to_string(), value.clone());
            Ok(value)
        })
    }

    fn list_to_string(&mut self, list: Ptr<Vec<Value>>) -> EvalResult<String> {
        let mut s = String::from("[");
        let mut is_first = true;

        for value in list.borrow().iter() {
            if is_first {
                is_first = false;
            } else {
                s.push_str(", ");
            }
            let value = self.value_to_string(value)?;
            s.push_str(&value);
        }

        s.push_str("]");
        Ok(s)
    }

    fn object_to_string(&mut self, obj: Ptr<Object>) -> EvalResult<String> {
        let obj = obj.borrow();
        let mut s = String::new();

        // Check if we have a name
        if let Some(value) = obj.get("type_name") {
            let name = self.value_to_string(&value)?;
            s.push_str(&name);
        }

        s.push('{');
        let mut is_first = true;

        let proto_str = "proto".to_owned();

        let proto = obj
            .proto
            .as_ref()
            .map(|proto| (&proto_str, Value::Object(proto.clone())));

        for (key, value) in obj
            .fields
            .iter()
            .map(|(key, value)| (key, value.clone()))
            .chain(proto.into_iter())
        {
            if key != "type_name" {
                if is_first {
                    is_first = false;
                } else {
                    s.push_str(", ");
                }
                let value = match value {
                    Value::Object(_) => "<Object>".to_owned(),
                    other => self.value_to_string(&other)?,
                };
                s.push_str(key);
                s.push_str(": ");
                s.push_str(&value);
            }
        }

        s.push_str("}");
        Ok(s)
    }

    fn init_types(&mut self) -> EvalResult<()> {
        let mut obj_proto = Object::new();

        obj_proto.define_method("to_string", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    let s = engine.object_to_string(obj.clone())?;
                    let s = Value::String(s.into());
                    engine.create_backed_object("String", s)
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        obj_proto.define_method("instance_of", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [Value::Object(of)]) => {
                    Ok(Value::Boolean(Object::instance_of(this, Some(of))))
                }
                (Value::Object(_), [x]) => Err(EvalError::type_mismatch("Object", x.type_of())),
                (Value::Object(_), xs) => Err(EvalError::arity_mismatch(1, xs.len())),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        obj_proto.define_method("index_get", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (parent, [Value::String(s)]) => Object::get_field(parent, s),
                (first, [second]) => Err(EvalError::type_mismatch(
                    "(Object, Number)",
                    format!("({}, {})", first.type_of(), second.type_of()),
                )),
                (_, xs) => Err(EvalError::arity_mismatch(1, xs.len())),
            }
        });

        obj_proto.define_method("index_get", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [Value::String(s), item]) => {
                    let mut this = this.borrow_mut();
                    this.set(s.as_ref(), item.clone());
                    Ok(Value::None)
                }
                (first, [second, _]) => Err(EvalError::type_mismatch(
                    "(Object, Number)",
                    format!("({}, {})", first.type_of(), second.type_of()),
                )),
                (_, xs) => Err(EvalError::arity_mismatch(2, xs.len())),
            }
        });

        let obj_proto = ptr(obj_proto);

        self.env.insert("Object", Value::Object(obj_proto.clone()));

        // Define list prototype
        let mut list_proto = Object::new();
        list_proto.set_proto(obj_proto.clone());

        list_proto.define_method("new", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    obj.borrow_mut().set("data", Value::List(ptr(Vec::new())));
                    Ok(Value::None)
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        list_proto.define_method("plus", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(obj), [addend]) => match obj.borrow().get("data") {
                    Some(Value::List(vec)) => {
                        let list = vec.as_ref().borrow();
                        let mut new_list = list.clone();
                        new_list.push(addend.clone());
                        let list = Value::List(ptr(new_list));
                        engine.create_backed_object("List", list)
                    }
                    Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                    None => Err(EvalError::key_not_found("data")),
                },
                (Value::Object(_), xs) => Err(EvalError::arity_mismatch(0, xs.len())),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        // [].to_string()
        list_proto.define_method("to_string", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    if let Some(val) = obj.borrow().get("data") {
                        match val {
                            Value::List(list) => {
                                let s = engine.list_to_string(list)?;
                                let s = Value::String(s.into());
                                engine.create_backed_object("String", s)
                            }
                            other => Err(EvalError::type_mismatch("List", other.type_of())),
                        }
                    } else {
                        Err(EvalError::key_not_found("data"))
                    }
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        list_proto.define_method("len", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    if let Some(val) = obj.borrow().get("data") {
                        match val {
                            Value::List(vec) => Ok(Value::Number(vec.borrow().len() as f64)),
                            other => Err(EvalError::type_mismatch("List", other.type_of())),
                        }
                    } else {
                        Err(EvalError::key_not_found("data"))
                    }
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        // [].get(i)
        list_proto.define_method("index_get", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [Value::Number(n)]) if is_usize(*n) => {
                    let n = *n as usize;
                    match this.borrow().get("data") {
                        Some(Value::List(vec)) => {
                            let list = vec.as_ref().borrow();
                            if n < list.len() {
                                Ok(list[n].clone())
                            } else {
                                Ok(Value::None)
                            }
                        }
                        Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                        None => Err(EvalError::key_not_found("data")),
                    }
                }
                (first, [second]) => Err(EvalError::type_mismatch(
                    "(Object, Number)",
                    format!("({}, {})", first.type_of(), second.type_of()),
                )),
                (_, xs) => Err(EvalError::arity_mismatch(1, xs.len())),
            }
        });

        // [].set(i, x)
        list_proto.define_method("index_set", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [Value::Number(n), item]) if is_usize(*n) => {
                    let n = *n as usize;
                    match this.borrow().get("data") {
                        Some(Value::List(vec)) => {
                            let mut list = vec.as_ref().borrow_mut();
                            if n < list.len() {
                                list[n] = item.clone();
                            }
                            Ok(Value::None)
                        }
                        Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                        None => Err(EvalError::key_not_found("data")),
                    }
                }
                (first, [second, _]) => Err(EvalError::type_mismatch(
                    "(Object, Number)",
                    format!("({}, {})", first.type_of(), second.type_of()),
                )),
                (_, xs) => Err(EvalError::arity_mismatch(2, xs.len())),
            }
        });

        // [].push(x)
        list_proto.define_method("push", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [value]) => match this.borrow().get("data") {
                    Some(Value::List(vec)) => {
                        let mut list = vec.borrow_mut();
                        list.push(value.clone());
                        Ok(Value::None)
                    }
                    Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                    None => Err(EvalError::key_not_found("data")),
                },
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
                (_, xs) => Err(EvalError::arity_mismatch(1, xs.len())),
            }
        });

        list_proto.define_method("pop", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), []) => match this.borrow().get("data") {
                    Some(Value::List(vec)) => {
                        let mut list = vec.borrow_mut();
                        Ok(list.pop().unwrap_or_else(|| Value::None))
                    }
                    Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                    None => Err(EvalError::key_not_found("data")),
                },
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
                (_, xs) => Err(EvalError::arity_mismatch(1, xs.len())),
            }
        });

        self.env
            .insert("List", Value::Object(ptr(list_proto.clone())));

        let mut str_proto = Object::new();
        str_proto.set_proto(obj_proto.clone());

        str_proto.define_method("new", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    obj.borrow_mut().set("data", Value::String("".into()));
                    Ok(Value::None)
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        str_proto.define_method("len", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    if let Some(val) = obj.borrow().get("data") {
                        match val {
                            Value::String(s) => Ok(Value::Number(s.len() as f64)),
                            other => Err(EvalError::type_mismatch("String", other.type_of())),
                        }
                    } else {
                        Err(EvalError::key_not_found("data"))
                    }
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        str_proto.define_method("char_code_at", |_, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [Value::Number(n)]) if is_usize(*n) => {
                    let n = *n as usize;
                    match this.borrow().get("data") {
                        Some(Value::String(s)) => {
                            if n < s.len() {
                                let ch = s.chars().skip(n).next().unwrap();
                                Ok(Value::Number(ch as u32 as f64))
                            } else {
                                Ok(Value::None)
                            }
                        }
                        Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                        None => Err(EvalError::key_not_found("data")),
                    }
                }
                (first, [second]) => Err(EvalError::type_mismatch(
                    "(Object, Number)",
                    format!("({}, {})", first.type_of(), second.type_of()),
                )),
                (_, xs) => Err(EvalError::arity_mismatch(1, xs.len())),
            }
        });

        str_proto.define_method("trim", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    if let Some(val) = obj.borrow().get("data") {
                        match val {
                            Value::String(s) => {
                                let new_str = s.trim().to_owned();
                                engine.make_string(new_str)
                            }
                            other => Err(EvalError::type_mismatch("String", other.type_of())),
                        }
                    } else {
                        Err(EvalError::key_not_found("data"))
                    }
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        str_proto.define_method("plus", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                // (Value::Object(obj), [addend]) => match obj.borrow().get("data") {
                //     Some(Value::String(s)) => {
                //         let s = s.as_ref();
                //         let new_str = format!("{}{}", s, addend);
                //         let s = Value::String(new_str.into());
                //         engine.create_backed_object("String", s)
                //     }
                //     Some(other) => Err(EvalError::type_mismatch("String", other.type_of())),
                //     None => Err(EvalError::key_not_found("data")),
                // },
                (a, [addend]) => {
                    let a = engine.value_to_string(a)?;
                    let b = engine.value_to_string(addend)?;
                    let sum = format!("{}{}", a, b);
                    let new_str = Value::String(sum.into());
                    engine.create_backed_object("String", new_str)
                }
                (Value::Object(_), xs) => Err(EvalError::arity_mismatch(0, xs.len())),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        str_proto.define_method("get", |engine, this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args) {
                (Value::Object(this), [Value::Number(n)]) if is_usize(*n) => {
                    let n = *n as usize;
                    match this.borrow().get("data") {
                        Some(Value::String(s)) => {
                            if n < s.len() {
                                let mut new_str = String::new();
                                let ch = s.chars().skip(n).next().unwrap();
                                new_str.push(ch);
                                engine.make_string(new_str)
                            } else {
                                Ok(Value::None)
                            }
                        }
                        Some(other) => Err(EvalError::type_mismatch("List", other.type_of())),
                        None => Err(EvalError::key_not_found("data")),
                    }
                }
                (first, [second]) => Err(EvalError::type_mismatch(
                    "(Object, Number)",
                    format!("({}, {})", first.type_of(), second.type_of()),
                )),
                (_, xs) => Err(EvalError::arity_mismatch(1, xs.len())),
            }
        });

        self.env
            .insert("String", Value::Object(ptr(str_proto.clone())));

        // obj_proto.borrow_mut().set(
        //     "type_name",
        //     self.create_backed_object("String", Value::String("Object".into()))?,
        // );

        // list_proto.set(
        //     "type_name",
        //     self.create_backed_object("String", Value::String("List".into()))?,
        // );

        // str_proto.set(
        //     "type_name",
        //     self.create_backed_object("String", Value::String("String".into()))?,
        // );

        Ok(())
    }

    pub fn make_string(&mut self, s: impl Into<String>) -> EvalResult<Value> {
        let buf = Value::String(s.into().into());
        self.create_backed_object("String", buf)
    }

    pub fn make_list(&mut self, vals: Vec<Value>) -> EvalResult<Value> {
        let buf = Value::List(ptr(vals));
        self.create_backed_object("List", buf)
    }

    fn get_proto(&self, name: impl AsRef<str>) -> EvalResult<Ptr<Object>> {
        match self.env.get(name.as_ref()) {
            Some(Value::Object(obj)) => Ok(obj.clone()),
            Some(other) => Err(EvalError::type_mismatch("Object", other.type_of())),
            None => Err(EvalError::undefined(name.as_ref())),
        }
    }

    fn define_unary_fn(&mut self, name: impl Into<String>, f: impl Fn(f64) -> f64 + 'static) {
        self.define_built_in(name, move |_, _, args| match args {
            [Value::Number(n)] => Ok(Value::Number(f(*n))),
            [x] => Err(EvalError::type_mismatch("Number", x.type_of())),
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });
    }

    fn define_binary_fn(&mut self, name: impl Into<String>, f: impl Fn(f64, f64) -> f64 + 'static) {
        self.define_built_in(name, move |_, _, args| match args {
            [Value::Number(a), Value::Number(b)] => Ok(Value::Number(f(*a, *b))),
            [x, Value::Number(_)] => Err(EvalError::type_mismatch("Number", x.type_of())),
            [Value::Number(_), x] => Err(EvalError::type_mismatch("Number", x.type_of())),
            xs => Err(EvalError::arity_mismatch(2, xs.len())),
        });
    }

    fn define_math_built_ins(&mut self) {
        self.define_unary_fn("__floor__", f64::floor);
        self.define_unary_fn("__ceil__", f64::ceil);
        self.define_unary_fn("__sin__", f64::sin);
        self.define_unary_fn("__cos__", f64::cos);
        self.define_unary_fn("__tan__", f64::tan);
        self.define_unary_fn("__asin__", f64::asin);
        self.define_unary_fn("__acos__", f64::acos);
        self.define_unary_fn("__atan__", f64::atan);
        self.define_unary_fn("__sqrt__", f64::sqrt);
        self.define_binary_fn("__atan2__", f64::atan2);
    }

    pub fn init(&mut self) -> EvalResult<()> {
        self.define_math_built_ins();

        self.define_built_in("__hash__", |_, _, args| match args {
            [value] => {
                let mut hasher = DefaultHasher::new();
                value.hash(&mut hasher);
                let output = hasher.finish() as f64;
                Ok(Value::Number(output))
            }
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });

        self.define_built_in("__exit__", |_, _, args| match args {
            [Value::Number(n)] if is_type!(*n, f64, i32) => {
                let n = *n as i32;
                std::process::exit(n);
            }
            [other] => Err(EvalError::type_mismatch("Number", other.type_of())),
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });

        self.define_built_in("__type_of__", |engine, _, args| match args {
            [arg] => engine.make_string(arg.type_of()),
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });

        self.define_built_in("__print__", |engine, _, args| {
            for arg in args {
                engine.print_value(arg)?;
                print!(" ");
            }
            println!();
            Ok(Value::None)
        });

        self.define_built_in("__input__", |engine, _, args| {
            use std::io::{self, prelude::*};

            match args {
                [value] => {
                    let s = engine.value_to_string(value)?;
                    print!("{}", s);
                    io::stdout().flush().expect("io error");
                }
                [] => {}
                xs => return Err(EvalError::arity_mismatch(1, xs.len())),
            }

            let mut buf = String::new();
            io::stdin().read_line(&mut buf).expect("io error");

            engine.make_string(buf)
        });

        self.define_built_in("__push_list__", |_, _, args| {
            match args.clone() {
                [Value::List(vec), item] => {
                    // Push to list
                    let mut list = vec.borrow_mut();
                    list.push(item.clone());
                    Ok(Value::None)
                }
                [other, _] => Err(EvalError::type_mismatch("List", other.type_of())),
                other => Err(EvalError::arity_mismatch(2, other.len())),
            }
        });

        self.define_built_in("__get_list__", |_, _, args| {
            match args.clone() {
                [Value::List(vec), Value::Number(n)] if is_usize(*n) => {
                    // Push to list
                    let list = vec.borrow();
                    list.get(*n as usize)
                        .cloned()
                        .ok_or_else(|| EvalError::key_not_found(format!("{}", n)))
                }
                [other, _] => Err(EvalError::type_mismatch("List", other.type_of())),
                other => Err(EvalError::arity_mismatch(2, other.len())),
            }
        });

        self.define_built_in("__set_list__", |_, _, args| match args.clone() {
            [Value::List(vec), Value::Number(n), value] if is_usize(*n) => {
                // Push to list
                let mut list = vec.borrow_mut();
                let n = *n as usize;
                if n < list.len() {
                    list[n] = value.clone();
                    Ok(Value::None)
                } else {
                    Err(EvalError::key_not_found(format!("{}", n)))
                }
            }
            [other, _] => Err(EvalError::type_mismatch("List", other.type_of())),
            other => Err(EvalError::arity_mismatch(2, other.len())),
        });

        self.define_built_in("__len_list__", |_, _, args| match args {
            [Value::List(vec)] => {
                // Push to list
                let list = vec.borrow();
                Ok(Value::Number(list.len() as f64))
            }
            [other] => Err(EvalError::type_mismatch("List", other.type_of())),
            other => Err(EvalError::arity_mismatch(2, other.len())),
        });

        self.define_built_in("__unix_time__", |_, _, args| match args.len() {
            0 => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let time = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("system clock error");
                Ok(Value::Number(time.as_millis() as f64))
            }
            n => Err(EvalError::arity_mismatch(0, n)),
        });

        self.define_built_in("__argv__", |engine, _, args| match args.len() {
            0 => {
                use std::env;

                let args: Vec<_> = env::args()
                    .map(|arg| engine.make_string(arg))
                    .collect::<Result<_, _>>()?;
                engine.make_list(args)
            }
            n => Err(EvalError::arity_mismatch(0, n)),
        });

        self.define_built_in("__eval__", |engine, _, args| match args {
            [value] => {
                let s = engine.value_to_string(value)?;
                // Parse string
                let lexer = Lexer::new(s.as_ref());
                let mut parser = Parser::new(lexer);

                let expr = parser.parse_expr()?;
                engine.evaluate(&expr)
            }
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });

        self.env
            .insert("__PI__", Value::Number(std::f64::consts::PI));
        self.env.insert("__E__", Value::Number(std::f64::consts::E));

        self.init_types()
    }

    fn start_loop(&mut self) -> usize {
        self.loop_depth += 1;
        return self.loop_depth;
    }

    fn stop_loop(&mut self) {
        if self.loop_depth > 0 {
            self.loop_depth -= 1;
        }
    }

    pub fn define_built_in<F>(&mut self, name: impl Into<String>, built_in: F)
    where
        F: Fn(&mut Engine, Option<Rc<Value>>, &[Value]) -> EvalResult<Value> + 'static,
    {
        let built_in = Callable::BuiltIn(Rc::new(built_in));
        let func = Function {
            capture: Rc::new(HashMap::new()),
            self_param: None,
            callable: built_in,
        };
        self.env.insert(name, Value::Function(func));
    }

    fn extract_closure(
        &mut self,
        expression: &Expression,
        ignore: &mut Capture,
    ) -> EvalResult<Frame> {
        let mut frame = HashMap::new();
        self.extract_closure_to_frame(&mut frame, expression, ignore)?;
        Ok(frame)
    }

    fn extract_closure_from_statement(
        &mut self,
        closure: &mut Frame,
        statement: &Statement,
        ignore: &mut Capture,
    ) -> EvalResult<()> {
        use Statement::*;
        match statement {
            FunctionDeclaration {
                parameters,
                body,
                identifier,
                ..
            } => {
                ignore.descend();
                ignore.insert(identifier);
                for param in parameters {
                    ignore.insert(param);
                }
                self.extract_closure_to_frame(closure, body, ignore)?;
                ignore.ascend();
            }
            Expression(expr) => {
                self.extract_closure_to_frame(closure, expr, ignore)?;
            }
            Assignment {
                value, parameter, ..
            } => {
                ignore.insert(parameter);
                self.extract_closure_to_frame(closure, value, ignore)?;
            }
            Reassignment { value, location } => {
                self.extract_closure_to_frame(closure, location, ignore)?;
                self.extract_closure_to_frame(closure, value, ignore)?;
            }
            Loop { body, .. } => {
                ignore.descend();
                for statement in body {
                    self.extract_closure_from_statement(closure, statement, ignore)?;
                }
                ignore.ascend();
            }
            While {
                condition, body, ..
            } => {
                ignore.descend();
                self.extract_closure_to_frame(closure, condition, ignore)?;
                for statement in body {
                    self.extract_closure_from_statement(closure, statement, ignore)?;
                }
                ignore.ascend();
            }
            For {
                item,
                iterator,
                body,
            } => {
                ignore.descend();
                ignore.insert(item);
                self.extract_closure_to_frame(closure, iterator, ignore)?;
                for statement in body {
                    self.extract_closure_from_statement(closure, statement, ignore)?;
                }
                ignore.ascend();
            }
            // ClassDeclaration {
            //     ..,
            // } => {

            // }
            _ => {}
        }
        Ok(())
    }

    fn extract_closure_to_frame(
        &mut self,
        closure: &mut Frame,
        expression: &Expression,
        ignore: &mut Capture,
    ) -> EvalResult<()> {
        use Expression::*;
        // println!("extract: {:?}", expression);
        // println!("ignore: {:?}", ignore);
        match expression {
            Member(obj, _) => {
                self.extract_closure_to_frame(closure, obj, ignore)?;
            }
            Identifier(path) => {
                if path.len() == 1 {
                    let ident = &path[0];
                    if !ignore.contains(ident) {
                        let value = self
                            .env
                            .get(ident)
                            .cloned()
                            .ok_or_else(|| EvalError::undefined(ident))
                            .map_err(|err| {
                                // println!("env: {:?}", self.env);
                                err
                            })?;
                        closure.insert(ident.into(), value);
                    }
                }
            }
            Call(expr, args) => {
                self.extract_closure_to_frame(closure, expr, ignore)?;
                for arg in args {
                    self.extract_closure_to_frame(closure, arg, ignore)?;
                }
            }
            List(exprs) => {
                for expr in exprs {
                    self.extract_closure_to_frame(closure, expr, ignore)?;
                }
            }
            Binary(_, a, b) => {
                self.extract_closure_to_frame(closure, a, ignore)?;
                self.extract_closure_to_frame(closure, b, ignore)?;
            }
            Unary(_, expr) => {
                self.extract_closure_to_frame(closure, expr, ignore)?;
            }
            Block(body, last) => {
                for statement in body {
                    self.extract_closure_from_statement(closure, statement, ignore)?;
                }
                self.extract_closure_to_frame(closure, last, ignore)?;
            }
            If(cond, then, otherwise) => {
                self.extract_closure_to_frame(closure, cond, ignore)?;
                self.extract_closure_to_frame(closure, then, ignore)?;
                self.extract_closure_to_frame(closure, otherwise, ignore)?;
            }
            Lambda(params, body) => {
                ignore.descend();
                for param in params {
                    ignore.insert(param);
                }
                self.extract_closure_to_frame(closure, body, ignore)?;
                ignore.ascend();
            }
            _ => {}
        }

        Ok(())
    }

    pub fn preload_module(&mut self, module: &Module) {
        let node = Node::trace(module);
        self.root.insert_child(node);
    }

    pub fn load_module(&mut self, module: &Module, bare_root: bool) -> EvalResult<()> {
        self.env.descend();
        self.cur_module = module.identifier.clone();
        if !bare_root {
            self.path.push(module.identifier.clone());
        }
        for statement in &module.body {
            if statement.is_top_level() {
                self.execute(statement)?;
            } else {
                return Err(EvalError::InvalidStatement);
            }
        }
        for child in &module.children {
            self.load_module(child, false)?;
        }
        if !bare_root {
            self.path.pop();
        }
        self.cur_module = "<None>".into();
        self.env.ascend();
        Ok(())
    }

    pub fn get_module_declaration(&self, identifier: &[String]) -> Option<&Value> {
        self.modules.get(identifier)
    }

    fn find_module_declaration(&self, identifier: &[String]) -> EvalResult<Value> {
        self.get_module_declaration(identifier)
            .cloned()
            // .unwrap_or_else(|| Value::Link(identifier.to_owned().into()))
            .ok_or_else(|| EvalError::undefined(identifier.join("::")))
    }

    fn find_module_declarations(&self, prefix: &[String]) -> Vec<Vec<String>> {
        self.root.find_items(prefix)
        // let res = self
        //     .modules
        //     .iter()
        //     .filter(|(path, _)| matches_prefix(prefix.iter(), path.iter()))
        //     .map(|(path, _)| path.clone())
        //     .collect();
        // res
    }

    fn define_module_declaration(&mut self, identifier: impl Into<String>, val: Value) {
        let mut item_path = self.path.clone();
        item_path.push(identifier.into());
        self.modules.insert(item_path, val);
    }

    pub fn run_main(&mut self) -> EvalResult<Value> {
        // Attempt to find main method
        let main_path = ["std".into(), "main".into(), "main".into()];

        let main = self.find_module_declaration(&main_path)?;

        match main {
            Value::Function(f) => self.call(&f, &[]),
            other => Err(EvalError::type_mismatch("Function", other.type_of())),
        }
    }

    fn is_looping(&self, depth: usize) -> bool {
        self.loop_depth >= depth
    }

    fn evaluate_identifier(&mut self, ident: &[String], lazy: bool) -> EvalResult<Value> {
        // Check if we have a path or simple identifier
        if ident.len() == 1 {
            // Handle case of simple identifier
            let ident = &ident[0];
            self.env
                .get(ident)
                .cloned()
                .ok_or_else(|| EvalError::undefined(ident))
                .map_err(|err| {
                    // println!("env: {:#?}", self.env);
                    println!("Error in module: {}", self.cur_module);
                    err
                })
                .and_then(|value| self.evaluate_value(&value, lazy))
        } else {
            // Look up the full path in modules
            if lazy {
                Ok(Value::Link(ident.to_vec().into()))
            } else {
                self.find_module_declaration(ident)
            }
        }
    }

    pub fn execute(&mut self, stmt: &Statement) -> EvalResult<()> {
        match stmt {
            Statement::For {
                item,
                iterator,
                body,
            } => {
                let mut iterator = self.evaluate(iterator)?;
                if !self.has_field(&iterator, "next") {
                    iterator = self.call_method(&iterator, "iter", &[])?;
                }

                let depth = self.start_loop();

                while self.is_looping(depth) {
                    self.env.descend();

                    // Get next value from iterator
                    let value = self.call_method(&iterator, "next", &[]);
                    match value {
                        Ok(Value::None) => {
                            // Finished iterating
                            self.stop_loop();
                        }
                        Ok(value) => {
                            // Bind to `item` and run body
                            self.env.insert(item, value);

                            for statement in body {
                                if let Err(e) = self.execute(statement) {
                                    self.stop_loop();
                                    self.env.ascend();
                                    return Err(e);
                                }
                            }
                        }
                        Err(err) => {
                            self.env.ascend();
                            self.stop_loop();
                            return Err(err);
                        }
                    }

                    self.env.ascend();
                }

                if self.loop_depth > depth {
                    self.stop_loop();
                }
                Ok(())
            }

            Statement::ClassDeclaration {
                visibility,
                identifier,
                parent,
                body,
            } => {
                // Initial object declaration
                let mut object = Object::new();

                if let Some(parent) = parent {
                    // Check parent
                    let parent = self.evaluate_identifier(parent, false)?;
                    if let Value::Object(parent_ptr) = parent {
                        object.set_proto(parent_ptr);
                    } else {
                        return Err(EvalError::type_mismatch("Object", parent.type_of()));
                    }
                } else {
                    let proto = self.get_proto("Object")?;
                    object.set_proto(proto);
                }

                let object = Value::Object(Rc::new(RefCell::new(object)));

                self.env.insert(identifier, object.clone());

                if visibility.is_visible() {
                    self.define_module_declaration(identifier, object.clone());
                }

                // Define methods
                self.env.descend();

                for statement in body {
                    self.execute(statement)?;
                }
                // Now we go over what has been defined in this level and move
                // into the object itself
                match &object {
                    Value::Object(obj) => {
                        let inner_obj = obj.clone();
                        let mut inner_obj = inner_obj.borrow_mut();

                        for frame in self.env.frames().next() {
                            for (key, value) in frame.iter() {
                                inner_obj.set(key, value.clone());
                            }
                        }
                    }
                    _ => unreachable!(),
                }

                self.env.ascend();

                // Check what was inserted
                // let inserted = self.lookup_string(identifier)?;

                // self.print_value(&inserted)?;
                Ok(())
            }
            Statement::Import { path, alias } => {
                // Check if wildcard import
                match path.last() {
                    Some(s) if s == "_" => {
                        // Import everything from that path
                        let prefix = &path[..path.len() - 1];
                        let items: Vec<_> = self
                            .find_module_declarations(prefix)
                            .into_iter()
                            .map(|item| item.clone())
                            .collect();

                        for path in items {
                            let name = path.last().unwrap();
                            let value = Value::Link(path.clone().into());
                            self.env.insert(name, value);
                        }
                        Ok(())
                    }
                    _ => {
                        let value = Value::Link(path.clone().into());
                        self.env.insert(alias, value);
                        Ok(())
                    }
                }
            }
            Statement::FunctionDeclaration {
                visibility,
                identifier,
                parameters,
                body,
            } => {
                let mut ignore = Capture::new();
                ignore.insert("self");
                ignore.insert(identifier);
                for param in parameters {
                    ignore.insert(param);
                }

                let capture = self.extract_closure(body, &mut ignore)?;
                // println!("capture ({}): {:?}", identifier, capture);

                let f = Value::Function(Function {
                    capture: Rc::new(capture),
                    self_param: None,
                    callable: Callable::RFunction(Rc::new(parameters.clone()), body.clone()),
                });
                self.env.insert(identifier, f.clone());
                if let Visibility::Public = visibility {
                    self.define_module_declaration(identifier, f);
                }
                Ok(())
            }
            Statement::Assignment {
                visibility,
                mutable,
                parameter,
                value,
            } => {
                let val = self.evaluate_maybe_lazy(value, true)?;
                self.env.insert(parameter, val.clone());
                if let Visibility::Public = visibility {
                    self.define_module_declaration(parameter, val);
                }
                Ok(())
            }
            Statement::Reassignment { location, value } => {
                match location {
                    Expression::Member(obj, field) => match self.evaluate(obj)? {
                        Value::Object(obj) => {
                            let value = self.evaluate(value)?;
                            obj.borrow_mut().set(field, value);
                            Ok(())
                        }
                        other => Err(EvalError::type_mismatch("Object", other.type_of())),
                    },
                    Expression::Index(obj, field) => {
                        let obj = self.evaluate(obj)?;
                        let key = self.evaluate(field)?;
                        let args = [key, self.evaluate(value)?];
                        self.call_method(&obj, "index_set", &args);
                        Ok(())
                    }
                    Expression::Identifier(ident) if ident.len() == 1 => {
                        let ident = &ident[0];
                        // Look up location
                        let value = self.evaluate(value)?;
                        if let Some(loc) = self.env.get_mut(ident) {
                            *loc = value;
                            Ok(())
                        } else {
                            // println!("env: {:?}", self.env);
                            Err(EvalError::undefined(ident)).map_err(|err| {
                                println!("Error in module: {}", self.cur_module);
                                err
                            })
                        }
                    }
                    other => {
                        println!("unimplemented: {:?}", other);
                        unimplemented!()
                    }
                }
            }
            Statement::Expression(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Statement::Loop { body } => {
                self.env.descend();
                let depth = self.start_loop();
                while self.is_looping(depth) {
                    for statement in body {
                        if let Err(e) = self.execute(statement) {
                            self.stop_loop();
                            self.env.ascend();
                            return Err(e);
                        }
                    }
                }
                self.stop_loop();
                self.env.ascend();
                Ok(())
            }
            Statement::While { condition, body } => {
                self.env.descend();
                let depth = self.start_loop();
                while self.is_looping(depth) {
                    // Check condition
                    let cond = self.evaluate(condition)?;
                    let cond = match cond {
                        Value::None | Value::Boolean(false) => false,
                        _ => true,
                    };

                    if !cond {
                        self.stop_loop();
                        continue;
                    }

                    for statement in body {
                        if let Err(e) = self.execute(statement) {
                            self.env.ascend();
                            return Err(e);
                        }
                    }
                }

                self.env.ascend();
                Ok(())
            }
            Statement::Break => {
                self.stop_loop();
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn evaluate_unary(&mut self, op: &UnaryOp, value: &Value) -> EvalResult<Value> {
        match op {
            UnaryOp::Minus => match value {
                Value::Number(n) => Ok(Value::Number(-n)),
                x => Err(EvalError::type_mismatch("Number", x.type_of())),
            },
            UnaryOp::Not => match value {
                Value::Boolean(b) => Ok(Value::Boolean(!b)),
                x => Err(EvalError::type_mismatch("Boolean", x.type_of())),
            },
            op => {
                println!("{:?} is not yet supported.", op);
                todo!()
            }
        }
    }

    fn evaluate_binary(&mut self, op: BinaryOp, a: &Value, b: &Value) -> EvalResult<Value> {
        match op {
            BinaryOp::DoubleAnd => match (a, b) {
                (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(*a && *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::DoubleOr => match (a, b) {
                (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(*a || *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Plus => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a + *b)),
                (obj @ Value::Object(_), addend) => {
                    // Use object's `plus` method
                    self.call_method(obj, "plus", &[addend.clone()])
                }
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Minus => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a - *b)),
                (obj @ Value::Object(_), addend) => {
                    // Use object's `plus` method
                    self.call_method(obj, "minus", &[addend.clone()])
                }
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Exponentiate => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a.powf(*b))),
                (obj @ Value::Object(_), addend) => {
                    // Use object's `plus` method
                    self.call_method(obj, "exponentiate", &[addend.clone()])
                }
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Times => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a * *b)),
                (obj @ Value::Object(_), addend) => {
                    // Use object's `plus` method
                    self.call_method(obj, "times", &[addend.clone()])
                }
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Divide => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a / *b)),
                (obj @ Value::Object(_), addend) => {
                    // Use object's `plus` method
                    self.call_method(obj, "divide", &[addend.clone()])
                }
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Mod => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a % *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::LT => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a < *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::LTE => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a <= *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::GT => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a > *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::GTE => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a >= *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Equal => match (a, b) {
                // Check for reference equality
                (Value::Object(a), Value::Object(b)) if ptr_eq(a.clone(), b.clone()) => {
                    Ok(Value::Boolean(true))
                }

                // Otherwise, if operand is an object, call `equal`
                (obj @ Value::Object(_), other) => {
                    // Use object's `equals` method
                    let res = self
                        .try_call_method(obj, "equals", &[other.clone()])?
                        .unwrap_or_else(|| Value::Boolean(false));
                    Ok(res)
                }
                (a, b) => Ok(Value::Boolean(a == b)),
            },
            BinaryOp::NotEqual => Ok(Value::Boolean(a != b)),
            _ => todo!(),
        }
    }

    fn create_backed_object(&mut self, proto: impl AsRef<str>, buffer: Value) -> EvalResult<Value> {
        let mut obj = Object::new();
        let proto = self.get_proto(proto)?;
        obj.set_proto(proto);
        obj.set("data", buffer);
        Ok(Value::Object(ptr(obj)))
    }

    fn capture_environment(&self, expr: &Expression) -> Frame {
        let mut frame = HashMap::new();
        frame
    }

    pub fn evaluate(&mut self, expr: &Expression) -> EvalResult<Value> {
        self.evaluate_maybe_lazy(expr, false)
    }

    pub fn evaluate_maybe_lazy(&mut self, expr: &Expression, lazy: bool) -> EvalResult<Value> {
        let value = match expr {
            Expression::ObjectLiteral(proto, key_values) => {
                let proto = self.evaluate(proto.as_ref())?;
                match proto {
                    Value::Object(proto) => {
                        let mut obj = Object::new();
                        obj.set_proto(proto);

                        let evaluated: Result<Vec<_>, _> = key_values
                            .iter()
                            .map(|(key, value)| match self.evaluate(value) {
                                Ok(value) => Ok((key.clone(), value)),
                                Err(err) => Err(err),
                            })
                            .collect();

                        let evaluated = evaluated?;

                        for (key, value) in evaluated {
                            obj.set(key, value);
                        }
                        let ptr = Rc::new(RefCell::new(obj));
                        let value = Value::Object(ptr);

                        Ok(value)
                    }
                    other => Err(EvalError::type_mismatch("Object", other.type_of())),
                }
            }
            Expression::List(exprs) => {
                let values: Vec<_> = exprs
                    .iter()
                    .map(|expr| self.evaluate(expr))
                    .collect::<Result<_, _>>()?;
                let list = Value::List(ptr(values));
                self.create_backed_object("List", list)
            }
            Expression::Block(statements, expr) => {
                self.env.descend();
                for statement in statements {
                    let res = self.execute(statement);
                    if let Err(err) = res {
                        self.env.ascend();
                        return Err(err);
                    }
                }
                let res = self.evaluate(expr);
                self.env.ascend();
                res
            }
            Expression::If(condition, then, otherwise) => {
                let condition = self.evaluate(condition)?;
                match condition {
                    Value::Boolean(b) if b => self.evaluate(then),
                    Value::Boolean(_) => self.evaluate(otherwise),
                    Value::None => self.evaluate(otherwise),
                    other => self.evaluate(then),
                }
            }
            Expression::Lambda(params, body) => {
                let mut ignore = Capture::new();
                ignore.insert("self");
                for param in params {
                    ignore.insert(param);
                }
                let closure = self.extract_closure(body, &mut ignore)?;
                Ok(Value::Function(Function {
                    capture: Rc::new(closure),
                    self_param: None,
                    callable: Callable::RFunction(Rc::new(params.clone()), body.as_ref().clone()),
                }))
            }
            Expression::Call(f, args) => {
                let f = self.evaluate(f)?;
                match f {
                    // Call function
                    Value::Function(f) => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|arg| self.evaluate(arg))
                            .collect::<Result<_, _>>()?;
                        self.call(&f, &args)
                    }

                    // If object, construct new object using it as proto
                    Value::Object(obj) => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|arg| self.evaluate(arg))
                            .collect::<Result<_, _>>()?;

                        let mut new_obj = Object::new();
                        new_obj.set_proto(obj.clone());
                        let new_obj = Value::Object(ptr(new_obj));
                        self.call_method(&new_obj, "new", &args);
                        Ok(new_obj)
                    }

                    other => {
                        println!("unimplemented: {} <{}>", other, other.type_of());
                        todo!()
                    }
                }
            }
            Expression::Binary(op, a, b) => {
                let a = self.evaluate(a)?;
                let b = self.evaluate(b)?;
                self.evaluate_binary(*op, &a, &b)
            }
            Expression::Unary(op, value) => {
                let value = self.evaluate(value)?;
                self.evaluate_unary(op, &value)
            }
            Expression::Member(parent, key) => {
                let parent = self.evaluate(parent)?;
                Object::get_field(&parent, key)
            }
            Expression::Identifier(ident) => self.evaluate_identifier(ident, lazy),
            Expression::String(s) => self.lookup_string(s),
            Expression::Boolean(b) => Ok(Value::Boolean(*b)),
            Expression::Int(n) => Ok(Value::Number(*n as f64)),
            Expression::Float(n) => Ok(Value::Number(*n)),
            Expression::None => Ok(Value::None),
            Expression::Tuple(_) => Ok(Value::None),
            Expression::Index(parent, field) => {
                let obj = self.evaluate(parent)?;
                let args = [self.evaluate(field)?];
                self.call_method(&obj, "index_get", &args)
            }
            other => {
                println!("unimplemented: {:?}", other);
                unimplemented!()
            }
        }?;
        self.evaluate_value(&value, lazy)
    }

    fn call(&mut self, f: &Function, args: &[Value]) -> EvalResult<Value> {
        match &f.callable {
            Callable::BuiltIn(func) => func(self, f.self_param.clone(), args),
            Callable::RFunction(params, body) => {
                // Capture environment
                self.env.descend();
                // println!("{:?}", f.capture);
                for (key, val) in f.capture.iter() {
                    self.env.insert(key, val.clone());
                }

                // Insert arguments
                self.env.descend();
                for (key, val) in params.iter().zip(args) {
                    self.env.insert(key, val.clone());
                }

                // Insert `self`
                let self_param = match &f.self_param {
                    Some(val) => val.as_ref().clone(),
                    None => Value::None,
                };
                self.env.insert("self", self_param);

                // Evaluate function
                let res = self.evaluate(body);

                // Cleanup
                self.env.ascend();
                self.env.ascend();

                res
            }
        }
    }

    fn evaluate_value(&mut self, val: &Value, lazy: bool) -> EvalResult<Value> {
        match val {
            // Recursively follow links to find a value
            Value::Link(path) if !lazy => {
                let value = self.find_module_declaration(path.as_ref())?;
                self.evaluate_value(&value, lazy)
            }
            other => Ok(other.clone()),
        }
    }

    pub fn value_to_string(&mut self, value: &Value) -> EvalResult<String> {
        if let Value::Object(obj) = value {
            // Check for `data` field
            let data = obj.borrow().get("data");
            if let Some(Value::String(s)) = data {
                return Ok(s.to_string());
            }
        }
        match value {
            obj @ Value::Object(_) => {
                let res = self.call_method(obj, "to_string", &[])?;
                self.value_to_string(&res)
            }
            other => return Ok(other.to_string()),
        }
    }

    pub fn print_value(&mut self, value: &Value) -> EvalResult<()> {
        // let to_print = match value {
        //     obj @ Value::Object(_) => self.call_method(obj, "to_string", &[])?,
        //     other => other.clone(),
        // };

        // match to_print {
        //     Value::String(s) => print!("{}", s),
        //     other => print!("{}", other),
        // }
        let to_print = self.value_to_string(value)?;
        print!("{}", to_print);
        Ok(())
    }

    pub fn has_field(&mut self, parent: &Value, field: impl AsRef<str>) -> bool {
        Object::get_field(parent, field).is_ok()
    }

    pub fn try_call_method(
        &mut self,
        parent: &Value,
        method: impl AsRef<str>,
        args: &[Value],
    ) -> EvalResult<Option<Value>> {
        let method = Object::try_get_field(parent, method)?;
        match method {
            Some(Value::Function(f)) => {
                let res = self.call(&f, args)?;
                Ok(Some(res))
            }
            Some(Value::Object(obj)) => {
                let mut new_obj = Object::new();
                new_obj.set_proto(obj.clone());
                let new_obj = Value::Object(ptr(new_obj));
                self.call_method(&new_obj, "new", &args);
                Ok(Some(new_obj))
            }
            Some(other) => Err(EvalError::type_mismatch("Function", other.type_of())),
            None => Ok(None),
        }
    }

    pub fn call_method(
        &mut self,
        parent: &Value,
        method: impl AsRef<str>,
        args: &[Value],
    ) -> EvalResult<Value> {
        let method = Object::get_field(parent, method)?;
        match method {
            Value::Function(f) => self.call(&f, args),
            Value::Object(obj) => {
                let mut new_obj = Object::new();
                new_obj.set_proto(obj.clone());
                let new_obj = Value::Object(ptr(new_obj));
                self.call_method(&new_obj, "new", &args);
                Ok(new_obj)
            }
            other => Err(EvalError::type_mismatch("Function", other.type_of())),
        }
    }
}

type Str = Rc<str>;

#[derive(Clone)]
pub enum Callable {
    BuiltIn(Rc<dyn Fn(&mut Engine, Option<Rc<Value>>, &[Value]) -> EvalResult<Value>>),
    RFunction(Rc<Vec<String>>, Expression),
}

#[derive(Clone, Debug)]
pub struct Function {
    capture: Rc<Frame>,
    self_param: Option<Rc<Value>>,
    callable: Callable,
}

type Ptr<T> = Rc<RefCell<T>>;

type WeakPtr<T> = Weak<RefCell<T>>;

#[inline]
fn ptr<T>(val: T) -> Ptr<T> {
    Rc::new(RefCell::new(val))
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Str),
    Symbol(Str),
    Object(Ptr<Object>),
    List(Ptr<Vec<Value>>),
    Function(Function),
    Link(Rc<[String]>),
    None,
}

fn ptr_eq<T, R: AsRef<T>>(a: R, b: R) -> bool {
    let a = a.as_ref() as *const _;
    let b = b.as_ref() as *const _;
    a == b
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use Value::*;
        match (self, other) {
            (Number(a), Number(b)) => a == b,
            (Boolean(a), Boolean(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Object(a), Object(b)) => ptr_eq(a.clone(), b.clone()),
            (List(a), List(b)) => ptr_eq(a.clone(), b.clone()),
            (Function(_), Function(_)) => false,
            (Link(_), Link(_)) => false,
            (None, None) => true,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    proto: Option<Ptr<Object>>,
    fields: Frame,
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (field, value) in self.fields.iter() {
            field.hash(state);
            value.hash(state);
        }
    }
}

impl Object {
    pub fn new() -> Object {
        Object {
            proto: None,
            fields: HashMap::new(),
        }
    }

    pub fn set_proto(&mut self, proto: Ptr<Object>) {
        self.proto = Some(proto);
    }

    pub fn try_get_super_field(
        parent: &Value,
        key: impl AsRef<str>,
        is_super: bool,
    ) -> EvalResult<Option<Value>> {
        match parent {
            Value::Object(obj) => {
                let obj = obj.borrow();
                let get = if is_super {
                    obj.get_super(key.as_ref())
                } else {
                    obj.get(key.as_ref())
                };
                let value = get.map(|field| match field {
                    Value::Function(mut f) => {
                        f.self_param = Some(Rc::new(parent.clone()));
                        Value::Function(f)
                    }
                    other => other,
                });
                Ok(value)
            }
            other => Err(EvalError::type_mismatch("Object", other.type_of())),
        }
    }

    pub fn try_get_field(parent: &Value, key: impl AsRef<str>) -> EvalResult<Option<Value>> {
        Object::try_get_super_field(parent, key, false)
    }

    pub fn get_super_field(
        parent: &Value,
        key: impl AsRef<str>,
        is_super: bool,
    ) -> EvalResult<Value> {
        let key = key.as_ref();
        Object::try_get_super_field(parent, key, is_super)
            .and_then(|value| value.ok_or_else(|| EvalError::key_not_found(key)))
    }

    pub fn get_field(parent: &Value, key: impl AsRef<str>) -> EvalResult<Value> {
        Object::get_super_field(parent, key, false)
        // match parent {
        //     Value::Object(obj) => obj
        //         .borrow()
        //         .get(key.as_ref())
        //         .ok_or_else(|| EvalError::key_not_found(key.as_ref()))
        //         .map(|field| match field {
        //             Value::Function(mut f) => {
        //                 f.self_param = Some(Rc::new(parent.clone()));
        //                 Value::Function(f)
        //             }
        //             other => other,
        //         }),
        //     other => Err(EvalError::type_mismatch("Object", other.type_of())),
        // }
    }

    pub fn obj_has_field(parent: Value, key: impl AsRef<str>) -> EvalResult<bool> {
        match parent {
            Value::Object(obj) => Ok(obj.borrow().get(key.as_ref()).is_some()),
            other => Err(EvalError::type_mismatch("Object", other.type_of())),
        }
    }

    pub fn has_field(&self, key: impl AsRef<str>) -> bool {
        self.get(key).is_some()
    }

    pub fn get_super(&self, key: impl AsRef<str>) -> Option<Value> {
        self.proto
            .as_ref()
            .and_then(|proto| proto.borrow().get(key).clone())
    }

    pub fn get(&self, key: impl AsRef<str>) -> Option<Value> {
        self.fields.get(key.as_ref()).cloned().or_else(|| {
            self.proto
                .as_ref()
                .and_then(|proto| proto.borrow().get(key).clone())
        })
    }

    pub fn set(&mut self, key: impl Into<String>, value: Value) {
        self.fields.insert(key.into(), value);
    }

    pub fn define_method<F>(&mut self, name: impl Into<String>, method: F)
    where
        F: Fn(&mut Engine, Option<Rc<Value>>, &[Value]) -> EvalResult<Value> + 'static,
    {
        let function = Function {
            capture: Rc::new(HashMap::new()),
            self_param: None,
            callable: Callable::BuiltIn(Rc::new(method)),
        };
        let function = Value::Function(function);
        self.set(name, function);
    }

    pub fn instance_of(obj: &Ptr<Object>, of: Option<&Ptr<Object>>) -> bool {
        if let Some(of) = of {
            if !ptr_eq(obj.clone(), of.clone()) {
                if let Some(proto) = obj.borrow().proto.as_ref() {
                    Object::instance_of(proto, Some(of))
                } else {
                    false
                }

            // Object::instance_of(obj.borrow().proto.as_ref(), of)
            } else {
                true
            }
        } else {
            false
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut is_rest = false;
        write!(f, "{{ ")?;
        for (key, value) in self.fields.iter() {
            if is_rest {
                write!(f, ", ")?;
            } else {
                is_rest = true;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, " }}")
    }
}

use std::fmt;

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Function>")
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) if *b => write!(f, "True"),
            Value::Boolean(_) => write!(f, "False"),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Object(obj) => write!(f, "{}", obj.as_ref().borrow()),
            Value::List(list) => {
                write!(f, "[")?;
                let list = list.as_ref().borrow();

                if list.len() > 0 {
                    let begin = &list[..list.len() - 1];
                    let end = &list[list.len() - 1..];

                    for val in begin {
                        write!(f, "{}, ", val)?;
                    }
                    for val in end {
                        write!(f, "{}", val)?;
                    }
                }

                write!(f, "]")
            }
            Value::Function(_) => write!(f, "<Function>"),
            Value::Link(path) => write!(f, "{}", path.join("::")),
            Value::None => write!(f, "None"),
        }
    }
}

impl Value {
    pub fn type_of(&self) -> &'static str {
        match self {
            Value::Number(_) => "Number",
            Value::Boolean(_) => "Boolean",
            Value::String(_) => "String",
            Value::Symbol(_) => "Symbol",
            Value::Object(_) => "Object",
            Value::List(_) => "List",
            Value::Function(_) => "Function",
            Value::Link(_) => "Link",
            Value::None => "None",
        }
    }
}

trait ClonePtr<T> {
    fn clone_ptr(&self) -> T;
}

impl<T: Clone> ClonePtr<T> for Ptr<T> {
    fn clone_ptr(&self) -> T {
        self.borrow().clone()
    }
}
