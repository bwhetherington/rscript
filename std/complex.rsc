pub let Complex = Object();

Complex.new = |real, complex| {
  self.r = real;
  self.c = complex;
};

Complex.plus = |other| Complex(self.r + other.r, self.c + other.c);

# Multiplication
# a+bi * c+di = (ac-bd)+(ac+bd)i
Complex.times = |other| {
  let p = self.r;
  let q = self.c;
  let r = other.r;
  let s = other.c;
  Complex(p * r - q * s, p * s + q * r)
};

