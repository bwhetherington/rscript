use crate::parser::*;
use std::cell::RefCell;
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    rc::Rc,
};

pub fn is_usize(num: f64) -> bool {
    (num as usize as f64) == num
}

#[derive(Debug)]
struct Capture {
    frames: Vec<HashSet<String>>,
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
    is_looping: bool,
    path: Vec<String>,
    modules: HashMap<Vec<String>, Value>,
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

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for EvalError {}

type EvalResult<T> = Result<T, EvalError>;

impl Engine {
    pub fn new() -> Engine {
        Engine {
            instructions: Vec::new(),
            env: Environment::new(),
            is_looping: false,
            path: Vec::new(),
            modules: HashMap::new(),
        }
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

        for (key, value) in &obj.fields {
            if key != "type_name" {
                if is_first {
                    is_first = false;
                } else {
                    s.push_str(", ");
                }
                let value = self.value_to_string(value)?;
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

        obj_proto.define_method("get", |_, this, args| {
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

        obj_proto.define_method("set", |_, this, args| {
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
        list_proto.define_method("get", |_, this, args| {
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
        list_proto.define_method("set", |_, this, args| {
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

    fn make_string(&mut self, s: impl Into<String>) -> EvalResult<Value> {
        let buf = Value::String(s.into().into());
        self.create_backed_object("String", buf)
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
        self.define_unary_fn("floor", f64::floor);
        self.define_unary_fn("ceil", f64::ceil);
        self.define_unary_fn("sin", f64::sin);
        self.define_unary_fn("cos", f64::cos);
        self.define_unary_fn("tan", f64::tan);
        self.define_unary_fn("asin", f64::asin);
        self.define_unary_fn("acos", f64::acos);
        self.define_unary_fn("atan", f64::atan);
        self.define_binary_fn("atan2", f64::atan2);
    }

    pub fn init(&mut self) -> EvalResult<()> {
        self.define_math_built_ins();

        self.define_built_in("type_of", |_, _, args| match args {
            [arg] => Ok(Value::String(arg.type_of().into())),
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });

        self.define_built_in("print", |engine, _, args| {
            for arg in args {
                engine.print_value(arg)?;
                print!(" ");
            }
            println!();
            Ok(Value::None)
        });

        self.define_built_in("push_list", |_, _, args| {
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

        self.define_built_in("get_list", |_, _, args| {
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

        self.define_built_in("set_list", |_, _, args| match args.clone() {
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

        self.define_built_in("len_list", |_, _, args| match args {
            [Value::List(vec)] => {
                // Push to list
                let list = vec.borrow();
                Ok(Value::Number(list.len() as f64))
            }
            [other] => Err(EvalError::type_mismatch("List", other.type_of())),
            other => Err(EvalError::arity_mismatch(2, other.len())),
        });

        self.define_built_in("unix_time", |_, _, args| match args.len() {
            0 => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let time = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("system clock error");
                Ok(Value::Number(time.as_millis() as f64))
            }
            n => Err(EvalError::arity_mismatch(0, n)),
        });

        self.define_built_in("argv", |engine, _, args| match args.len() {
            0 => {
                use std::env;

                let args: Vec<_> = env::args().map(|arg| Value::String(arg.into())).collect();
                let list = Value::List(ptr(args));
                let mut value = Object::new();
                let proto = engine.get_proto("List")?;
                value.set_proto(proto);
                value.set("data", list);

                Ok(Value::Object(ptr(value)))
            }
            n => Err(EvalError::arity_mismatch(0, n)),
        });

        self.define_built_in("eval", |engine, _, args| match args {
            [Value::String(s)] => {
                // Parse string
                let lexer = Lexer::new(s.as_ref());
                let mut parser = Parser::new(lexer);

                let expr = parser.parse_expr()?;
                engine.evaluate(&expr)
            }
            [x] => Err(EvalError::type_mismatch("String", x.type_of())),
            xs => Err(EvalError::arity_mismatch(1, xs.len())),
        });

        self.init_types()
    }

    #[inline]
    fn start_loop(&mut self) {
        self.is_looping = true;
    }

    #[inline]
    fn stop_loop(&mut self) {
        self.is_looping = false;
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
            Reassignment { value, .. } => {
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
                            })
                            .and_then(|value| self.evaluate_value(&value))?;
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

    pub fn load_module(&mut self, module: &Module, bare_root: bool) -> EvalResult<()> {
        self.env.descend();
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
        self.env.ascend();
        Ok(())
    }

    pub fn get_module_declaration(&self, identifier: &[String]) -> Option<&Value> {
        self.modules.get(identifier)
    }

    fn find_module_declaration(&self, identifier: &[String]) -> EvalResult<Value> {
        self.get_module_declaration(identifier)
            .cloned()
            .ok_or_else(|| EvalError::undefined(identifier.join("::")))
    }

    fn define_module_declaration(&mut self, identifier: impl Into<String>, val: Value) {
        let mut item_path = self.path.clone();
        item_path.push(identifier.into());
        self.modules.insert(item_path, val);
    }

    pub fn run_main(&mut self) -> EvalResult<Value> {
        // Attempt to find main method
        let main_path = ["main".into(), "main".into()];

        let main = self.find_module_declaration(&main_path)?;

        match main {
            Value::Function(f) => {
                // println!("calling main");
                // println!("{:?}", f.capture);
                self.call(&f, &[])
            }
            other => Err(EvalError::type_mismatch("Function", other.type_of())),
        }
    }

    pub fn execute(&mut self, stmt: &Statement) -> EvalResult<()> {
        match stmt {
            Statement::For {
                item,
                iterator,
                body,
            } => {
                let iterator = self.evaluate(iterator)?;
                self.start_loop();

                while self.is_looping {
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

                self.stop_loop();
                Ok(())
            }

            Statement::ClassDeclaration {
                visibility,
                identifier,
                parent,
                body,
            } => {
                // Foo

                todo!()
            }
            Statement::Import { path, alias } => {
                let value = Value::Link(path.clone().into());
                self.env.insert(alias, value);
                Ok(())
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
                let val = self.evaluate(value)?;
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
                        self.call_method(&obj, "set", &args);
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
                            Err(EvalError::undefined(ident))
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
                self.start_loop();
                while self.is_looping {
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
                self.start_loop();
                while self.is_looping {
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
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Times => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a * *b)),
                (a, b) => Err(EvalError::type_mismatch(a.type_of(), b.type_of())),
            },
            BinaryOp::Divide => match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(*a / *b)),
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
            BinaryOp::Equal => Ok(Value::Boolean(a == b)),
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
        match expr {
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

                    _ => todo!(),
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
            Expression::Identifier(ident) => {
                // Check if we have a path or simple identifier
                let value = if ident.len() == 1 {
                    // Handle case of simple identifier
                    let ident = &ident[0];
                    self.env
                        .get(ident)
                        .cloned()
                        .ok_or_else(|| EvalError::undefined(ident))
                        .map_err(|err| {
                            // println!("env: {:#?}", self.env);
                            err
                        })
                        .and_then(|value| self.evaluate_value(&value))?
                } else {
                    // Look up the full path in modules
                    self.find_module_declaration(ident)?
                };

                Ok(value)
            }
            Expression::String(s) => {
                let buffer = Value::String(s.to_string().into());
                self.create_backed_object("String", buffer)
            }
            Expression::Boolean(b) => Ok(Value::Boolean(*b)),
            Expression::Int(n) => Ok(Value::Number(*n as f64)),
            Expression::Float(n) => Ok(Value::Number(*n)),
            Expression::None => Ok(Value::None),
            Expression::Tuple(_) => Ok(Value::None),
            Expression::Index(parent, field) => {
                let obj = self.evaluate(parent)?;
                let args = [self.evaluate(field)?];
                self.call_method(&obj, "get", &args)
            }
            other => {
                println!("unimplemented: {:?}", other);
                unimplemented!()
            }
        }
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

    fn evaluate_value(&mut self, val: &Value) -> EvalResult<Value> {
        match val {
            Value::Link(path) => self.find_module_declaration(path.as_ref()),
            other => Ok(other.clone()),
        }
    }

    fn value_to_string(&mut self, value: &Value) -> EvalResult<String> {
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

    pub fn call_method(
        &mut self,
        parent: &Value,
        method: impl AsRef<str>,
        args: &[Value],
    ) -> EvalResult<Value> {
        let method = Object::get_field(parent, method)?;
        match method {
            Value::Function(f) => self.call(&f, args),
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

fn ptr_eq<T>(a: &Ptr<T>, b: &Ptr<T>) -> bool {
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
            (Object(a), Object(b)) => ptr_eq(a, b),
            (List(a), List(b)) => ptr_eq(a, b),
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

    pub fn get_field(parent: &Value, key: impl AsRef<str>) -> EvalResult<Value> {
        match parent {
            Value::Object(obj) => obj
                .borrow()
                .get(key.as_ref())
                .ok_or_else(|| EvalError::key_not_found(key.as_ref()))
                .map(|field| match field {
                    Value::Function(mut f) => {
                        f.self_param = Some(Rc::new(parent.clone()));
                        Value::Function(f)
                    }
                    other => other,
                }),
            other => Err(EvalError::type_mismatch("Object", other.type_of())),
        }
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
