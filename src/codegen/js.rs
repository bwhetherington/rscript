use std::fmt;
use std::rc::Rc;

type Str = Rc<str>;

type Definition = (Str, JsValue);

pub enum JsBody {
    Statements(Vec<JsStatement>),
    Value(JsValue),
}

pub enum JsStatement {}

pub enum JsValue {
    Number(f64),
    Boolean(bool),
    String(Str),
    Object(Vec<Definition>),
    Call(Box<JsValue>, Box<JsBody>),
    Null,
    Undefined,
}
