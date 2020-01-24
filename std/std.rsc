import std::iter::Range;
import std::rand::rng;
import std::io::println;

pub let Slice = List();

Slice.new = |list, from, to| {
  self.list = list;
  self.from = from;
  self.to = to;
};

Slice.len = || self.to - self.from;

Slice.index_get = |i| self.list[self.from + i];

Slice.index_set = |i, value| {
  self.list[self.from + i] = value;
};

Slice.to_string = || 
  self.list.iter()
    .slice(self.from, self.to)
    .list()
    .to_string();

List.slice = |from, to| Slice(self, from, to);

List.times = |num| {
  let list = [];
  for _ in Range(0, num) {
    for x in self {
      list.push(x);
    };
  };
  list
};

List.copy = || {
  let new_list = [];
  for item in self.iter() {
    new_list.push(item);
  };
  new_list
};

List.contains = |item| {
  let is_contained = False;
  for self_item in self.iter() {
    if item == self_item then {
      is_contained = True;
      break;
    };
  };
  is_contained
};

List.shuffle_rng = |gen| {
  let len = self.len();
  let gen = gen.ints(0, len);
  let new_list = [];
  let picked = [-1];
  for i in Range(0, len) {
    # Pick index
    let index = -1;
    while picked.contains(index) {
      index = gen.next();
    };
    picked.push(index);
    new_list.push(self[index]);
  };
  new_list
};

List.shuffle = || self.shuffle_rng(rng());

List.rev = || {
  let len = self.len();
  let output = [];
  for i in Range(0, len) {
    let index = len - i - 1;
    output.push(self[index]);
  };
  output
};

String.equals = |other| {
  if self.len() == other.len() then {
    # If strings are of the same length, check all pairs of chars for equality
    let output = True;
    for pair in self.char_codes().zip(other.char_codes()) {
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

  for ch in self {
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