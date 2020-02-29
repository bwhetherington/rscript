import std::iter::Range;

String.equals = |other| {
  if self.len() == other.len() then {
    # If strings are of the same length, check all pairs of chars for equality
    let output = True;
    for pair in self.char_codes().zip(other.char_codes()) do {
      let a = pair[0];
      let b = pair[1];
      if a != b then {
        # If any pair of chars is unequal, the strings are unequal
        output = False;
        break;
      };
    };
    output
  } else {
    False
  }
};

String.char_codes = || {
  # Somewhat awkward how we have to deliberately remember `self`
  let this = self;

  Range(0, self.len())
    .map(|index| this.char_code_at(index)) # Because `self` refers to the iterator here
};

String.slice = |from, to| {
  self.iter().skip(from).take(to - from).sum()
};

String.split = |delim| {
  let strs = [];
  let cur = "";

  for ch in self do {
    if ch == delim then {
      if cur.len() > 0 then {
        strs.push(cur);
        cur = "";
      };
    } else {
      cur = cur + ch;
    };
  };

  if cur.len() > 0 then {
    strs.push(cur);
  };

  strs
};