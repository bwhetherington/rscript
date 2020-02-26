# import std::io::println;
# import std::rand::rng;
import std::prelude::_;
import std::iter::_;

pub fn fib_generator() = {
  let state = Object();
  state.a = 0;
  state.b = 1;
  FunctionIterator(state, |state| {
    let next = state.a + state.b;
    let cur = state.a;
    state.a = state.b;
    state.b = next;
    cur
  })
};

# class Test {
#   op new(value) = {
#     self.value = value;
#   };

#   fn test() = self.value;
# };

pub fn main() = {
  for x in fib_generator().take(10) do {
    println(x);
  };
};