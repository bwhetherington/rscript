pub class Complex {
  op new(r, c) = {
    self._r = r;
    self._c = c;
  };

  fn real() = self._r;

  fn complex() = self._c;

  op plus(other) = Complex(self._r + other._r, self._c + other._c);

  op times(other) = {
    let p = self._r;
    let q = self._c;
    let r = other._r;
    let s = other._c;
    Complex(p * r - q * s, p * s + q * r)
  };
};

