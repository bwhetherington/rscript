pub let Iterator = Object();

# Base implementations
Iterator.next = || None;
Iterator.filter = |pred| FilterIterator(self, pred);
Iterator.map = |map| MapIterator(self, map);
Iterator.take = |num| TakeIterator(self, num);
Iterator.skip = |num| SkipIterator(self, num);
Iterator.zip = |iter| ZipIterator(self, iter);
Iterator.iter = || self;
Iterator.slice = |from, to| self.skip(from).take(to - from);

Iterator.list = || {
  let list = [];
  let value = self.next();
  while value != None {
    list.push(value);
    value = self.next();
  };
  list
};

Iterator.fold = |acc, func| {
  for x in self {
    acc = func(acc, x);
  };
  acc
};

Iterator.reduce = |func| {
  let acc = self.next();
  for x in self {
    acc = func(acc, x);
  };
  acc
};

Iterator.sum = || self.reduce(|acc, x| acc + x);

Iterator.for_each = |func| {
  for x in self {
    func(x);
  };
};

pub let IndexIterator = Iterator();

IndexIterator.new = |list| {
  self.list = list;
  self.end = list.len();
  self.index = 0;
};

IndexIterator.peek = || {
  self.list[self.index]
};

IndexIterator.next = || {
  if self.index < self.end then {
    let value = self.list[self.index];
    self.index = self.index + 1;
    value
  }
};

List.iter = || {
  IndexIterator(self)
};

String.iter = || {
  IndexIterator(self)
};

pub let FilterIterator = Iterator();

FilterIterator.new = |iter, pred| {
  self.iter = iter;
  self.pred = pred;
};

FilterIterator.next = || {
  let value = self.iter.next();
  if value then {
    if self.pred(value) then {
      value
    } else {
      self.next()
    }
  }
};

pub let MapIterator = Iterator();

MapIterator.new = |iter, map| {
  self.iter = iter;
  self.map = map;
};

MapIterator.next = || {
  let value = self.iter.next();
  if value then {
    self.map(value)
  }
};

pub let TakeIterator = Iterator();

TakeIterator.new = |iter, num| {
  self.iter = iter;
  self.max = num;
  self.cur = 0;
};

TakeIterator.next = || {
  if self.cur < self.max then {
    self.cur = self.cur + 1;
    self.iter.next()
  } else {
    None
  }
};

pub let SkipIterator = Iterator();

SkipIterator.new = |iter, num| {
  self.iter = iter;
  for i in Range(0, num) {
    self.iter.next();
  };
};

SkipIterator.next = || self.iter.next();

pub let Range = Iterator();

Range.new = |from, to| {
  self.from = from;
  self.to = to;
};

Range.next = || {
  if self.from < self.to then {
    let val = self.from;
    self.from = val + 1;
    val
  }
};

let ZipIterator = Iterator();

ZipIterator.new = |a, b| {
  self.a = a;
  self.b = b;
};

ZipIterator.next = || {
  let a = self.a.next();
  let b = self.b.next();

  if a != None && b != None then {
    [a, b]
  }
};

pub let FunctionIterator = Iterator();

FunctionIterator.new = |state, func| {
  self.func = func;
  self.state = state;
};

FunctionIterator.next = || {
  self.func(self.state)
};