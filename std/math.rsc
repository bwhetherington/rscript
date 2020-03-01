import std::iter::Range;

pub let PI = __PI__;
pub let E = __E__;

pub let floor = __floor__;
pub let ceil = __ceil__;
pub let sin = __sin__;
pub let cos = __cos__;
pub let tan = __tan__;
pub let asin = __asin__;
pub let acos = __acos__;
pub let atan = __atan__;
pub let atan2 = __atan2__;

# Produces the nth fibonacci number.
pub fn fibonacci(x) = {
  let fibs = [0, 1];
  for i in Range(2, x + 1) do {
    let val = fibs[i - 1] + fibs[i - 2];
    fibs.push(val);
  };
  fibs[x]
};

pub fn sqrt(num) = num ** 0.5;

pub class Vec2 {
  # Initializes a `Vec2` with the specified x and components.
  op new(x, y) = {
    self.x = x;
    self.y = y;
  };

  # Initializes a `Vec2` with the specified x and y-coordinates in Cartesian
  # space.
  fn cartesian(x, y) = Vec2(x, y);

  # Initializes a `Vec2` with the specified angle and radius in polar 
  # coordinates.
  fn polar(r, theta) =
    Vec2(r * cos(theta), r * sin(theta));

  op plus(other) = 
    Vec2(self.x + other.x, self.y + other.y);

  op minus(other) = 
    Vec2(self.x - other.x, self.y - other.y);

  op times(other) = 
    Vec2(self.x * other, self.y * other);

  op divide(other) = 
    Vec2(self.x / other, self.y / other);

  fn angle() = 
    atan2(self.y, self.x);

  fn magnitude() = 
    sqrt(self.x * self.x + self.y * self.y);
  
  op to_string() = "(" + self.x + ", " + self.y + ")";
};

pub class Either {
  op new(a, b) = {
    self.a = a;
    self.b = b;
  };

  op plus(other) = {
    
  };
};