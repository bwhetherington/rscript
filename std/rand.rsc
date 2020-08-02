import std::iter::Iterator;
import std::math::floor;
import std::proc::time;

let A = 1664525;
let C = 1013904223;
let M = 4294967296;

pub class Rng ext Iterator {
  op new(seed) = {
    self._current = seed % M;
    self.next();
  };

  op next() = {
    self._current = (A * self._current + C) % M;
    self._current / M
  };

  fn ints(min, max) = self.map(fn(r) = floor(r * (max - min)) + min);
};

pub fn rng() = Rng(time());

let GLOBAL_RNG = rng();

pub fn random() = GLOBAL_RNG.next();

pub fn rand_int(low, high) = GLOBAL_RNG.ints(low, high).next();

fn max(a, b) = if b > a then b else a;

fn min(a, b) = if b < a then b else a;

pub fn advantage_rolls() = {
  let seed = time();
  let a = Rng(seed).ints(1, 21);
  let b = Rng(seed + 1).ints(1, 21);
  a.zip_with(b, max)
};

pub fn disadvantage_rolls() = {
  let seed = time();
  let a = Rng(seed).ints(1, 21);
  let b = Rng(seed + 1).ints(1, 21);
  a.zip_with(b, min)
};