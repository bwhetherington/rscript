import std::iter::Iterator;
import std::math::floor;
import std::proc::time;

let A = 1664525;
let C = 1013904223;
let M = 4294967296;

pub class Rng ext Iterator {
  op new(seed) = {
    self.current = seed % M;
    self.next();
  };

  op next() = {
    self.current = (A * self.current + C) % M;
    self.current / M
  };

  fn ints(min, max) = self.map(|r| floor(r * (max - min)) + min);
};

pub fn rng() = Rng(time());