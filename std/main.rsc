# import std::io::println;
# import std::rand::rng;
import std::prelude::_;
import std::iter::_;
import std::hash::_;

pub fn fib_generator() = {
  let state = Object { a: 0, b: 1 };
  FunctionIterator(state, |state| {
    let next = state.a + state.b;
    let cur = state.a;
    state.a = state.b;
    state.b = next;
    cur
  })
};

pub fn main() = {
  let set = HashSet();

  set.insert(10);
  set.insert(20);
  set.insert(30);
  
  let sum = [1,2,3].iter() + [4,5,6].iter();
  println(sum.list());
};