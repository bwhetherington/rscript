# Produces the nth fibonacci number.
pub fn fibonacci(x) = {
  let fibs = [0, 1];
  for i in iter::Range(2, x + 1) {
    let val = fibs[i - 1] + fibs[i - 2];
    fibs.push(val);
  };
  fibs[x]
};

pub let Vec2 = Object();

Vec2.new = |x, y| {
  self.x = x;
  self.y = y;
};

Vec2.plus = |other| math::Vec2(self.x + other.x, self.y + other.y);

pub fn add(x, y) = {
  x.flat_map(|x| y.flat_map(|y| monad::Some(x + y)))
};