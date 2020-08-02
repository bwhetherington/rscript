use crate::engine::{vm2::Value, Ptr, Str, WeakPtr};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Object {
    proto: Option<WeakPtr<Object>>,
    fields: HashMap<Str, Value>,
}

impl Object {
    pub fn new(proto: Option<&Ptr<Object>>) -> Object {
        let proto = proto.map(|proto| Rc::downgrade(proto));
        let fields = HashMap::new();
        Object { proto, fields }
    }

    pub fn get(&self, field: impl AsRef<str>) -> Option<Value> {
        self.fields.get(field.as_ref()).cloned().or_else(|| {
            self.proto
                .as_ref()
                .and_then(|proto| proto.upgrade())
                .and_then(|proto| proto.borrow().get(field))
        })
    }

    pub fn set(&mut self, field: impl Into<Str>, value: Value) {
        self.fields.insert(field.into(), value);
    }

    pub fn get_field_location(&mut self, field: impl Into<Str>) -> &mut Value {
        self.fields.entry(field.into()).or_insert(Value::None)
    }
}
