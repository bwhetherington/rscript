let A = 1664525;
let C = 1013904223;
let M = 4294967296;

pub let Rng = iter::Iterator();

Rng.new = |seed| {
  self.current = seed % M;
  self.next();
};

Rng.next = || {
  self.current = (A * self.current + C) % M;
  self.current / M
};

Rng.next_int = |min, max| {
  let num = self.next();
  let diff = max - min;
  floor(num * diff) + min
};

pub let DEFAULT_RNG = Rng(1234);