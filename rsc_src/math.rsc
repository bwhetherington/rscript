pub fn factorial(x) = {
  let prod = 1;
  for i in iter::Range(1, x + 1) {
    prod = prod * i;
  };
  prod
};

# pub fn factorial_recursive(x) = {
#     if x < 1 then 1 else x * factorial_recursive(x - 1)
# };

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