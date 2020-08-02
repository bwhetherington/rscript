import std::iter::Range;
import std::hash::HashSet;

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

fn is_object(obj, T) = std::type_of(obj) && obj.instance_of(T);

# Produces the nth fibonacci number.
pub fn fibonacci(x) = {
  let fibs = [0, 1];
  for i in Range(2, x + 1) do {
    let val = fibs[i - 1] + fibs[i - 2];
    fibs.push(val);
  };
  fibs[x]
};

pub fn fib_rec(x) = {
  if x < 2
  then x
  else fib_rec(x - 1) + fib_rec(x - 2)
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
  fn cartesian(x, y) = self(x, y);

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

pub class Union {
  op new(items) = {
    if is_object(items, HashSet) then {
      self._set = items;
    } else {
      let set = HashSet();
      for item in items do {
        set.insert(item);
      };
      self._set = set;
    };
  };

  fn iter() = self._set.iter();

  fn map_fn(f, operand) = {
    # Check if addend is itself a union
    if std::type_of(operand) == "Object" then {
      if operand.instance_of(Union) then {
        let new_set = [];
        for x in self do {
          for y in operand do {
            new_set.push(f(x, y));
          };
        };
        Union(new_set)
      } else {
        Union(self.iter().map(fn(x) = f(x, operand)))
      }
    } else {
      Union(self.iter().map(fn(x) = f(x, operand)))
    }
  };

  op plus(addend) = self.map_fn(fn(x, y) = x + y, addend);

  op minus(subtrahend) = self.map_fn(fn(x, y) = x - y, subtrahend);

  op to_string() = " | ".join(self.set.iter().list());
};

pub fn plus_minus(x, y) = Union([x + y, x - y]);