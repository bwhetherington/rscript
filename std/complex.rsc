pub class Complex {
  op new(r, c) = {
    self.r = r;
    self.c = c;
  };

  op plus(other) = Complex(self.r + other.r, self.c + other.c);

  op times(other) = {
    let p = self.r;
    let q = self.c;
    let r = other.r;
    let s = other.c;
    Complex(p * r - q * s, p * s + q * r)
  };
};

