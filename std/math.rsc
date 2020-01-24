import std::iter::Range;

# Produces the nth fibonacci number.
pub fn fibonacci(x) = {
  let fibs = [0, 1];
  for i in Range(2, x + 1) {
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

Vec2.polar = |r, theta| Vec2(r * cos(theta), r * sin(theta));

Vec2.plus = |other| Vec2(self.x + other.x, self.y + other.y);

Vec2.minus = |other| Vec2(self.x - other.x, self.y - other.y);

Vec2.times = |other| Vec2(self.x * other, self.y * other);

Vec2.divide = |other| Vec2(self.x / other, self.y / other);

Vec2.angle = || atan2(self.y, self.x);

Vec2.magnitude = || (self.x * self.x + self.y * self.y) ** 0.5;

pub let PI = __PI__;
pub let E = __E__;

Vec2.to_string = || "Vec2(" + self.x + ", " + self.y + ")";