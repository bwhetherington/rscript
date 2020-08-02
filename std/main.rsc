import std::prelude::_;
import std::rand::_;

pub class Box {
  op new(val) = {
    self._val = val;
  };

  fn get() = self._val;

  fn set(val) = {
    let old = self._val;
    self._val = val;
    old
  };

  op to_string() = "Box(" + self.get() + ")";
};

let test = Box(10);

fn foo() = {
  println(test.get());
  test.set(test.get() + 1);
};

pub fn main() = {
};