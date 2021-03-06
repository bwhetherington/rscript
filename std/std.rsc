import std::iter::Range;
import std::rand::rng;
import std::io::println;

pub class Slice ext List {
  op new(list, from, to) = {
    self.list = list;
    self.from = from;
    self.to = to;
  };

  fn _index(i) = {
    if i < 0 then self.len() + i else i
  };

  op index_get(i) = self.list[self.from + self._index(i)];

  op index_set(i, value) = {
    self.list[self.from + self._index(i)] = value;
  };

  op to_string() = self.iter().list().to_string();

  fn iter() = self.list.iter().slice(self.from, self.to);

  fn len() = self.to - self.from;
};

List.slice = fn(from, to) = Slice(self, from, to);

List.times = fn(num) = {
  let list = [];
  for _ in Range(0, num) do {
    for x in self do {
      list.push(x);
    };
  };
  list
};

List.copy = fn() = {
  let new_list = [];
  for item in self.iter() do {
    new_list.push(item);
  };
  new_list
};

List.push = fn(x) = std::core::list_push(self.data, x);

List.index_get = fn(i) = std::core::list_get(self.data, i);

List.index_set = fn(i, val) = std::core::list_set(self.data, i, val);

List.len = fn() = std::core::list_len(self.data);

List.contains = fn(item) = {
  let is_contained = False;
  for self_item in self.iter() do {
    if item == self_item then {
      is_contained = True;
      break;
    };
  };
  is_contained
};

List.shuffle_rng = fn(gen) = {
  let len = self.len();
  let gen = gen.ints(0, len);
  let new_list = [];
  let picked = [-1];
  for i in Range(0, len) do {
    # Pick index
    let index = -1;
    while picked.contains(index) do {
      index = gen.next();
    };
    picked.push(index);
    new_list.push(self[index]);
  };
  new_list
};

List.shuffle = fn() = self.shuffle_rng(rng());

List.rev = fn() = {
  let len = self.len();
  let output = [];
  for i in Range(0, len) do {
    let index = len - i - 1;
    output.push(self[index]);
  };
  output
};

List.with_size = fn(init, len) = {
  let list = [];
  for i in Range(0, len) do {
    list.push(init());
  };
  list
};