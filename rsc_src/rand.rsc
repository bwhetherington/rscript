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

Rng.ints = |min, max| self.map(|r| floor(r * (max - min)) + min);

pub let DEFAULT_RNG = rand::Rng(unix_time());