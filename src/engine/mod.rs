mod env;
mod ignore;
mod node;
mod obj;
mod vm;
pub mod vm2;

pub use env::*;
pub use ignore::*;
pub use node::*;
pub use obj::*;
pub use vm::*;

use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

pub type Ptr<T> = Rc<RefCell<T>>;
pub type WeakPtr<T> = Weak<RefCell<T>>;
pub type Str = Rc<str>;

pub fn ptr<T>(el: T) -> Ptr<T> {
    Rc::new(RefCell::new(el))
}
