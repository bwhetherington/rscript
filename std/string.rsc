import std::iter::Range;
import std::iter::IndexIterator;
import std::io::println;

String.equals = fn(other) = {
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

String.char_codes = fn() = {
  # Somewhat awkward how we have to deliberately remember `self`
  let this = self;

  Range(0, self.len())
    .map(fn(index) = this.char_code_at(index)) # Because `self` refers to the iterator here
};

String.slice = fn(from, to) = {
  self.iter().skip(from).take(to - from).sum()
};

String.split = fn(delim) = {
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

String.join = fn(items) = {
  let s = "";
  if items.len() > 0 then {
    for i in Range(0, items.len() - 1) do {
      s = s + items[i] + self;
    };
    s = s + items[items.len() - 1];
  };
  s
};

String.from = fn(items, delim) = {
  let s = "";
  if items.len() > 0 then {
    for i in Range(0, items.len() - 1) do {
      s = s + items[i] + delim;
    };
    s = s + items[items.len() - 1];
  };
  s
};

String.copy = fn() = self;

String.char_codes = fn() = {
  let str = self;
  Range(0, self.len()).map(fn(i) = str.char_code_at(i))
};

String.char_view = fn() = CharView(self);

class CharView {
  op new(str) = {
    self._str = str;
  };

  fn len() = self._str.len();

  op index_get(i) = {
    if i < 0 then {
      self._str.char_code_at(self.len() + i)
    } else {
      self._str.char_code_at(i)
    }
  };

  fn iter() = IndexIterator(self);
};