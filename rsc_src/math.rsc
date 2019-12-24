pub fn factorial(x) = {
  let output = 1;
  while x > 0 {
    output = output * x;
    x = x - 1;
  };
  output
};

pub fn factorial_recursive(x) = {
    if x < 1 then 1 else x * factorial_recursive(x - 1)
};

pub fn fibonacci(x) = {
  let fibs = [0, 1];
  let i = 2;
  while i <= x {
    let val = fibs[i - 1] + fibs[i - 2];
    fibs.push(val);
    i = i + 1;
  };
  fibs[x]
};

pub let Point = Object();

Point.new = |x, y| {
  self.x = x;
  self.y = y;
};

Point.to_string = || {
  "(" + self.x + ", " + self.y + ")"
};

Point.add = |other| Point(self.x + other.x, self.y + other.y);