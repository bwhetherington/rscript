use crate::parser::*;
use std::cell::RefCell;
use std::{collections::HashMap, error::Error, rc::Rc};

pub fn is_usize(num: f64) -> bool {
    (num as usize as f64) == num
}

type Frame = HashMap<String, Value>;

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

    fn init_types(&mut self) {
        let mut obj_proto = Object::new();
        obj_proto.define_method("to_string", |this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => Ok(Value::String(obj.borrow().to_string().into())),
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        obj_proto.define_method("get", |this, args| {
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

        obj_proto.define_method("set", |this, args| {
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
        list_proto.set_proto(obj_proto);

        // [].to_string()
        list_proto.define_method("to_string", |this, args| {
            let this = this.ok_or_else(|| EvalError::undefined("self"))?.clone();
            match (this.as_ref(), args.len()) {
                (Value::Object(obj), 0) => {
                    if let Some(val) = obj.borrow().get("data") {
                        Ok(Value::String(val.to_string().into()))
                    } else {
                        Err(EvalError::key_not_found("data"))
                    }
                }
                (Value::Object(_), len) => Err(EvalError::arity_mismatch(0, len)),
                (other, _) => Err(EvalError::type_mismatch("Object", other.type_of())),
            }
        });

        list_proto.define_method("len", |this, args| {
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
        list_proto.define_method("get", |this, args| {
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
        list_proto.define_method("set", |this, args| {
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
        list_proto.define_method("push", |this, args| {
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

        self.env.insert("List", Value::Object(ptr(list_proto)));
    }

    fn get_proto(&self, name: impl AsRef<str>) -> EvalResult<Ptr<Object>> {
        match self.env.get(name.as_ref()) {
            Some(Value::Object(obj)) => Ok(obj.clone()),
            Some(other) => Err(EvalError::type_mismatch("Object", other.type_of())),
            None => Err(EvalError::undefined(name.as_ref())),
        }
    }

    pub fn init(&mut self) {
        self.define_built_in("print", |_, args| {
            for arg in args {
                print!("{} ", arg);
            }
            println!();
            Ok(Value::None)
        });
        self.define_built_in("push_list", |_, args| {
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

        self.define_built_in("get_list", |_, args| {
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

        self.define_built_in("set_list", |_, args| match args.clone() {
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

        self.define_built_in("len_list", |_, args| match args {
            [Value::List(vec)] => {
                // Push to list
                let list = vec.borrow();
                Ok(Value::Number(list.len() as f64))
            }
            [other] => Err(EvalError::type_mismatch("List", other.type_of())),
            other => Err(EvalError::arity_mismatch(2, other.len())),
        });

        self.init_types();
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
        F: Fn(Option<Rc<Value>>, &[Value]) -> EvalResult<Value> + 'static,
    {
        let built_in = Callable::BuiltIn(Rc::new(built_in));
        let func = Function {
            capture: Rc::new(HashMap::new()),
            self_param: None,
            callable: built_in,
        };
        self.env.insert(name, Value::Function(func));
    }

    pub fn load_module(&mut self, module: &Module, bare_root: bool) -> EvalResult<()> {
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
            Value::Function(f) => self.call(&f, &[]),
            other => Err(EvalError::type_mismatch("Function", other.type_of())),
        }
    }

    pub fn execute(&mut self, stmt: &Statement) -> EvalResult<()> {
        match stmt {
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
                let f = Value::Function(Function {
                    capture: Rc::new(HashMap::new()),
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
                self.env.insert(parameter, val);
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
                            Err(EvalError::undefined(ident))
                        }
                    }
                    _ => unimplemented!(),
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
                            self.env.ascend();
                            return Err(e);
                        }
                    }
                }
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

    pub fn evaluate(&mut self, expr: &Expression) -> EvalResult<Value> {
        match expr {
            Expression::List(exprs) => {
                let values: Vec<_> = exprs
                    .iter()
                    .map(|expr| self.evaluate(expr))
                    .collect::<Result<_, _>>()?;
                let list = Value::List(ptr(values));

                let mut value = Object::new();
                let proto = self.get_proto("List")?;
                value.set_proto(proto);
                value.set("data", list);

                Ok(Value::Object(ptr(value)))
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
            Expression::Lambda(params, body) => Ok(Value::Function(Function {
                capture: Rc::new(HashMap::new()),
                self_param: None,
                callable: Callable::RFunction(Rc::new(params.clone()), body.as_ref().clone()),
            })),
            Expression::Call(f, args) => {
                let f = self.evaluate(f)?;
                match f {
                    Value::Function(f) => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|arg| self.evaluate(arg))
                            .collect::<Result<_, _>>()?;
                        self.call(&f, &args)
                    }
                    _ => todo!(),
                }
            }
            Expression::Binary(op, a, b) => {
                let a = self.evaluate(a)?;
                let b = self.evaluate(b)?;
                self.evaluate_binary(*op, &a, &b)
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
                        .and_then(|value| self.evaluate_value(&value))?
                } else {
                    // Look up the full path in modules
                    self.find_module_declaration(ident)?
                };

                Ok(value)
            }
            Expression::String(s) => Ok(Value::String(s.to_string().into())),
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
            Callable::BuiltIn(func) => func(f.self_param.clone(), args),
            Callable::RFunction(params, body) => {
                // Capture environment
                self.env.descend();
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

    fn call_method(
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
    BuiltIn(Rc<dyn Fn(Option<Rc<Value>>, &[Value]) -> EvalResult<Value>>),
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
        F: Fn(Option<Rc<Value>>, &[Value]) -> EvalResult<Value> + 'static,
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
            Value::String(s) => write!(f, "{}", s),
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
            Value::Function(func) => write!(f, "{:?}", func),
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
            Value::None => "Null",
        }
    }
}
