pub let Iterator = Object();

# Base implementations
Iterator.next = || None;
Iterator.filter = |pred| FilterIterator(self, pred);
Iterator.map = |map| MapIterator(self, map);
Iterator.take = |num| TakeIterator(self, num);

Iterator.collect = || {
  let list = [];
  let value = self.next();
  while value != None {
    list.push(value);
    value = self.next();
  };
  list
};

let ListIterator = Iterator();

ListIterator.new = |list| {
  self.list = list;
  self.index = 0;
};

ListIterator.peek = || {
  self.list[self.index]
};

ListIterator.next = || {
  let value = self.list[self.index];
  self.index = self.index + 1;
  value
};

List.iter = || {
  ListIterator(self)
};

let FilterIterator = Iterator();

FilterIterator.new = |iter, pred| {
  self.iter = iter;
  self.pred = pred;
};

FilterIterator.next = || {
  let value = self.iter.next();
  if value == None then {
    None
  } else {
    if self.pred(value) then {
      value
    } else {
      self.next()
    }
  }
};

let MapIterator = Iterator();

MapIterator.new = |iter, map| {
  self.iter = iter;
  self.map = map;
};

MapIterator.next = || {
  let value = self.iter.next();
  if value != None then {
    self.map(value)
  } else {
    None
  }
};

let TakeIterator = Iterator();

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