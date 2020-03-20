import std::prelude::_;
import std::math::_;
import std::iter::_;

fn fib_gen() = {
  let state = Object {
    term1: 1,
    term2: 1,
  };
  FunctionIterator(state, |state| {
    let prev = state.term1;
    let next = state.term1 + state.term2;
    state.term1 = state.term2;
    state.term2 = next;
    prev
  })
};

pub fn main() = {
  let sum = fib_gen()
    .filter(|x| x % 2 == 0)
    .take_while(|x| x < 4e6)
    .sum();
  println(sum);
};