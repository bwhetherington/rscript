import std::prelude::_;
import std::math::_;
import std::iter::_;

fn is_prime(x, primes) = {
  !primes.iter().any(|prime| x % prime == 0)
};

fn prime_gen() = {
  let state = Object {
    primes: [],
    current: 1,
  };
  FunctionIterator(state, |state| {
    let primes = state.primes;
    let current = state.current + 1;

    # Iterate up from the current prime
    while !is_prime(current, primes) do {
      current = current + 1;
    };

    # Add the new prime
    primes.push(current);
    current
  })
};

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
  for prime in prime_gen().take(10) do {
    println(prime);;
  };
};