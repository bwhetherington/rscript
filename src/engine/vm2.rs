#![allow(unused)]
use crate::{
    engine::{obj::Object, ptr, Environment, Frame, Ignore, Node, Ptr, Str, Weak},
    parser::{DExpression, DStatement},
};
use std::collections::HashMap;
use std::{fmt, rc::Rc};

type Buffer = Vec<Value>;

#[derive(Clone)]
struct Builtin(Rc<dyn Fn(&[Value], &mut Engine) -> EvalResult<Value>>);

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Builtin>")?;
        Ok(())
    }
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Builtin>")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Character(char),
    Link(Rc<[Str]>),
    Buffer(Ptr<Buffer>),
    Object(Ptr<Object>),
    Function(Option<Ptr<Object>>, Rc<Function>),
    Builtin(Builtin),
    None,
}

#[derive(Clone, Debug)]
pub struct Function {
    capture: Frame,
    params: Vec<Str>,
    body: DExpression,
}

impl Value {
    pub fn string_buffer(s: &str) -> Buffer {
        s.chars().map(|ch| Value::Character(ch)).collect()
    }

    pub fn type_of(&self) -> &'static str {
        match self {
            Value::None => "None",
            Value::Number(..) => "Number",
            Value::Boolean(..) => "Boolean",
            Value::Buffer(..) => "Buffer",
            Value::Object(..) => "Object",
            Value::Link(..) => "Link",
            Value::Function(..) | Value::Builtin(..) => "Function",
            Value::Character(..) => "Character",
        }
    }
}

#[derive(Clone, Debug)]
pub enum EvalError {
    Static(&'static str),
    Undefined(Str),
    TypeMismatch(Str, Str),
    ArityMismatch(usize, usize),
    Custom(Str),
    InvalidBreak,
}

macro_rules! eval_error {
    ($($fmt:tt)*) => {{
        let s = format!($($fmt)*);
        $crate::engine::vm2::EvalError::custom(s)
    }};
}

impl EvalError {
    pub fn undefined(s: impl Into<Str>) -> EvalError {
        EvalError::Undefined(s.into())
    }

    pub fn type_mismatch(expected: impl Into<Str>, found: impl Into<Str>) -> EvalError {
        EvalError::TypeMismatch(expected.into(), found.into())
    }

    pub fn arity_mismatch(expected: usize, found: usize) -> EvalError {
        EvalError::ArityMismatch(expected, found)
    }

    pub fn custom(text: impl Into<Str>) -> EvalError {
        EvalError::Custom(text.into())
    }

    pub fn undefined_property(prop: impl AsRef<str>) -> EvalError {
        eval_error!("undefined property: `{}`", prop.as_ref())
    }
}

type EvalResult<T> = Result<T, EvalError>;

pub struct Engine {
    module_items: HashMap<Str, Value>,
    env: Environment,
    module_root: Node,
}

impl Engine {
    /// Looks up the specified key in the environment. An undefined error is
    /// thrown if the key is not found.
    fn lookup(&self, key: impl AsRef<str>) -> EvalResult<&Value> {
        self.env
            .get(key.as_ref())
            .ok_or_else(|| EvalError::Undefined(key.as_ref().into()))
    }

    fn lookup_prototype(&self, name: impl AsRef<str>) -> EvalResult<&Ptr<Object>> {
        let proto = self.lookup(name)?;
        match proto {
            Value::Object(obj) => Ok(obj),
            other => Err(EvalError::type_mismatch("Object", other.type_of())),
        }
    }

    fn buffer_object(&self, proto: impl AsRef<str>, buf: Buffer) -> EvalResult<Value> {
        let proto = self.lookup_prototype(proto)?;
        let mut obj = Object::new(Some(proto));
        obj.set("__buffer__", Value::Buffer(ptr(buf)));
        Ok(Value::Object(ptr(obj)))
    }

    fn make_list(&self, items: Vec<Value>) -> EvalResult<Value> {
        self.buffer_object("__List__", items)
    }

    fn make_string(&self, s: Str) -> EvalResult<Value> {
        self.buffer_object("__String__", Value::string_buffer(s.as_ref()))
    }

    fn invoke(&mut self, func: &Value, args: &[Value]) -> EvalResult<Value> {
        match func {
            Value::Object(obj) => {
                let new = Object::new(Some(&obj));
                let res = Value::Object(ptr(new));
                // Call new on object if new method exists
                self.try_call_method(&res, "new", args)?;

                Ok(res)
            }
            Value::Builtin(Builtin(f)) => f(args, self),
            Value::Function(self_value, data) => {
                self.env.descend();
                if let Some(self_value) = self_value {
                    self.env.insert("self", Value::Object(self_value.clone()));
                }
                for (key, value) in &data.capture {
                    self.env.insert(key.clone(), value.clone());
                }
                let param_bindings = data
                    .params
                    .iter()
                    .map(|key| key.clone())
                    .zip(args.into_iter());
                for (key, value) in param_bindings {
                    self.env.insert(key, value.clone());
                }
                let res = self.evaluate(&data.body)?;
                self.env.ascend();
                Ok(res)
            }
            _ => Err(EvalError::type_mismatch("Function", func.type_of())),
        }
    }

    fn try_get_field(&mut self, obj: &Value, field: &str) -> EvalResult<Option<Value>> {
        match obj {
            Value::Object(obj) => Ok(obj.borrow().get(field)),
            other => Err(EvalError::type_mismatch("Object", other.type_of())),
        }
    }

    fn get_field(&mut self, obj: &Value, field: &str) -> EvalResult<Value> {
        self.try_get_field(obj, field)?
            .ok_or_else(|| EvalError::undefined_property(field))
    }

    fn try_call_method(
        &mut self,
        obj: &Value,
        method: &str,
        args: &[Value],
    ) -> EvalResult<Option<Value>> {
        let method = self.try_get_field(obj, method)?;
        match method {
            Some(method) => {
                let res = self.invoke(&method, args)?;
                Ok(Some(res))
            }
            None => Ok(None),
        }
    }
    fn call_method(&mut self, obj: &Value, method: &str, args: &[Value]) -> EvalResult<Value> {
        self.try_call_method(obj, method, args)?
            .ok_or_else(|| EvalError::undefined_property(method))
    }

    /// Attempts to resolve the location given by the specified expression.
    fn mutate_location(
        &mut self,
        loc: &DExpression,
        f: impl FnOnce(&mut Value) -> EvalResult<()>,
    ) -> EvalResult<()> {
        match loc {
            DExpression::Member(obj, field) => {
                let parent = self.evaluate(obj)?;
                match parent {
                    Value::Object(obj) => {
                        let mut obj = obj.borrow_mut();
                        let mut field = obj.get_field_location(field.clone());
                        f(field)
                    }
                    // Type error
                    other => Err(EvalError::type_mismatch("Object", other.type_of())),
                }
            }
            DExpression::Identifier(ident) => {
                let mut location = self.env.get_mut(ident.as_ref());
                location
                    .ok_or_else(|| EvalError::undefined(ident.clone()))
                    .and_then(|location| f(location))
            }
            other => Err(EvalError::Static("a valid LHS must be a member expression, an index expression, or an identifier expression")),
        }
    }

    fn extract_closure(&self, expr: &DExpression) -> EvalResult<Frame> {
        let mut to = Frame::new();
        let mut ignore = Ignore::new();
        self.extract_closure_expression(expr, &mut to, &mut ignore)?;
        Ok(to)
    }

    fn extract_closure_expression(
        &self,
        expr: &DExpression,
        to: &mut Frame,
        ignore: &mut Ignore,
    ) -> EvalResult<()> {
        match expr {
            DExpression::Block(body, expr) => {
                ignore.descend();
                for stmt in body {
                    self.extract_closure_statement(stmt, to, ignore)?;
                }
                self.extract_closure_expression(expr, to, ignore)?;
                ignore.ascend();
            }
            DExpression::Identifier(ident) => {
                // Look up value if it hasn't been ignored
                if !ignore.contains(ident.as_ref()) {
                    let value = self.lookup(ident)?;
                    to.insert(ident.clone(), value.clone());
                }
            }
            DExpression::Function(params, body) => {
                ignore.descend();
                for param in params {
                    ignore.insert(param.clone());
                }
                self.extract_closure_expression(body, to, ignore)?;
                ignore.ascend();
            }
            _ => (),
        }
        Ok(())
    }

    fn extract_closure_statement(
        &self,
        stmt: &DStatement,
        to: &mut Frame,
        ignore: &mut Ignore,
    ) -> EvalResult<()> {
        match stmt {
            DStatement::Assignment(_, name, value) => {
                ignore.insert(name.as_ref());
                self.extract_closure_expression(value, to, ignore)?;
            }
            DStatement::Expression(expr) => {
                self.extract_closure_expression(expr, to, ignore)?;
            }
            DStatement::Loop(body) => {
                ignore.descend();
                for stmt in body {
                    self.extract_closure_statement(stmt, to, ignore)?;
                }
                ignore.ascend();
            }
            _ => (),
        }
        Ok(())
    }

    // fn find_module_declarations(&self, prefix: &[Str]) -> Vec<Vec<String>> {
    //     self.module_root.find_items(prefix)
    // }

    fn execute_internal(&mut self, stmt: &DStatement, in_loop: bool) -> EvalResult<bool> {
        match stmt {
            // DStatement::GlobImport(prefix) => {
            //     // Import everything from that path
            //     let items: Vec<_> = self
            //         .find_module_declarations(prefix)
            //         .into_iter()
            //         .map(|item| item.clone())
            //         .collect();

            //     for path in items {
            //         let name = path.last().unwrap();
            //         let value = Value::Link(path.clone().into());
            //         self.env.insert(name, value);
            //     }
            //     Ok(())
            // }
            DStatement::Expression(expr) => {
                self.evaluate(expr)?;
                Ok(true)
            }
            DStatement::Assignment(vis, name, expr) => {
                let value = self.evaluate(expr)?;
                self.env.insert(name.as_ref(), value);
                Ok(true)
            }
            DStatement::Loop(body) => {
                self.env.descend();
                for stmt in body {
                    if !self.execute_internal(stmt, true)? {
                        break;
                    }
                }
                self.env.ascend();
                Ok(true)
            }
            DStatement::Break if in_loop => Ok(false),
            DStatement::Break => Err(EvalError::InvalidBreak),
            DStatement::Reassignment(loc, val) => {
                let val = self.evaluate(val)?;
                self.mutate_location(loc, |loc| {
                    *loc = val;
                    Ok(())
                })?;
                Ok(true)
            }
            _ => todo!(),
        }
    }

    pub fn execute(&mut self, stmt: &DStatement) -> EvalResult<()> {
        self.execute_internal(stmt, false)?;
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &DExpression) -> EvalResult<Value> {
        self.evaluate_internal(expr, false)
    }

    pub fn evaluate_internal(&mut self, expr: &DExpression, lazy: bool) -> EvalResult<Value> {
        match expr {
            DExpression::Block(body, res) => {
                self.env.descend();
                for stmt in body {
                    self.execute(stmt)?;
                }
                let res = self.evaluate(res)?;
                self.env.ascend();
                Ok(res)
            }
            DExpression::String(s) => self.make_string(s.clone()),
            DExpression::List(exprs) => {
                let vals: Result<Vec<Value>, _> =
                    exprs.iter().map(|expr| self.evaluate(expr)).collect();
                let vals = vals?;
                self.make_list(vals)
            }
            DExpression::Character(ch) => Ok(Value::Character(*ch)),
            DExpression::Boolean(b) => Ok(Value::Boolean(*b)),
            DExpression::Number(n) => Ok(Value::Number(*n)),
            DExpression::If(cond, then, otherwise) => match self.evaluate(cond)? {
                Value::Boolean(true) => self.evaluate(then),
                Value::Boolean(false) => self.evaluate(otherwise),
                other => Err(EvalError::type_mismatch("Boolean", other.type_of())),
            },
            DExpression::Member(obj, field) => {
                let parent = self.evaluate(obj)?;
                match parent {
                    Value::Object(obj) => {
                        let parent = obj.borrow();
                        let value = parent
                            .get(field)
                            .ok_or_else(|| EvalError::undefined_property(field))?;
                        let value = match value {
                            Value::Function(_, f) => Value::Function(Some(obj.clone()), f),
                            other => other,
                        };

                        Ok(value)
                    }
                    other => Err(EvalError::type_mismatch("Object", other.type_of())),
                }
            }
            DExpression::TryMember(obj, field) => {
                let parent = self.evaluate(obj)?;
                match parent {
                    Value::None => Ok(Value::None),
                    Value::Object(obj) => {
                        let parent = obj.borrow();
                        let value = parent
                            .get(field)
                            .ok_or_else(|| EvalError::undefined_property(field))?;
                        let value = match value {
                            Value::Function(_, f) => Value::Function(Some(obj.clone()), f),
                            other => other,
                        };

                        Ok(value)
                    }
                    other => Err(EvalError::type_mismatch("Object", other.type_of())),
                }
            }
            f @ DExpression::Function(..) => {
                let capture = self.extract_closure(f)?;
                match f {
                    DExpression::Function(params, body) => {
                        let data = Function {
                            capture,
                            params: params.clone(),
                            body: body.as_ref().clone(),
                        };
                        Ok(Value::Function(None, Rc::new(data)))
                    }
                    _ => unreachable!(), // As we just checked it
                }
            }
            DExpression::Apply(f, args) => {
                let func = self.evaluate(f)?;
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Result<_, _>>()?;
                self.invoke(&func, &args)
            }
            DExpression::Identifier(i) => self.lookup(i).map(|val| val.clone()),
            DExpression::Path(path) => todo!(),
            _ => todo!(),
        }
    }
}
