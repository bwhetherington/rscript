pub let Fraction = Object();

Fraction.simplify = || {
  let g = gcd(self.num, self.den);
  self.num = self.num / g;
  self.den = self.den / g;
  if self.den < 0 then {
    self.num = -self.num;
    self.den = -self.den;
  } else None;
};

Fraction.new = |num, den| {
  self.num = num;
  self.den = den;
  self.simplify();
};

Fraction.plus = |other| {
  let new_num = self.num * other.den + self.den * other.num;
  let new_den = self.den * other.den;
  Fraction(new_num, new_den)
};

pub fn gcd(a, b) = if b == 0 then a else main::gcd(b, a % b);

Fraction.to_string = || {
  if self.den == 1 then {
    "" + self.num
  } else {
    "" + self.num + "/" + self.den
  }
};

pub fn main() = {
  let fract = Fraction(4, 8);
  print(fract);
};