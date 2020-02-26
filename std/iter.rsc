pub class Iterator {
  op next() = None;

  fn filter(pred) = FilterIterator(self, pred);

  fn map(f) = MapIterator(self, f);

  fn take(count) = TakeIterator(self, count);

  fn skip(count) = SkipIterator(self, count);

  fn zip(iter) = ZipIterator(self, iter);

  fn iter() = self;

  fn slice(from, to) = self.skip(from).take(to - from);

  fn list() = {
    let list = [];
    for value in self do {
      list.push(value);
    };
    list
  };

  fn fold(acc, func) = {
    for x in self do {
      acc = func(acc, x);
    };
    acc
  };

  fn reduce(func) = {
    let acc = self.next();
    self.fold(acc, func)
  };

  fn sum() = self.fold(0, |a, b| a + b);

  fn for_each(func) = {
    for x in self do {
      func(x);
    };
  };
};

class IndexIterator : Iterator {
  op new(list) = {
    self.list = list;
    self.end = list.len();
    self.index = 0;
  };

  op next() = {
    if self.index < self.end then {
      let value = self.list[self.index];
      self.index = self.index + 1;
      value
    } 
  };

  fn peek() = self.list[self.index];
};

List.iter = || {
  IndexIterator(self)
};

String.iter = || {
  IndexIterator(self)
};

class FilterIterator : Iterator {
  op new(iter, pred) = {
    self.iter = iter;
    self.pred = pred;
  };

  op next() = {
    let value = self.iter.next();
    if value then {
      if self.pred(value) then {
        value
      } else {
        self.next()
      }
    }
  };
};

class MapIterator : Iterator {
  op new(iter, map) = {
    self.iter = iter;
    self.map = map;
  };

  op next() = {
    let value = self.iter.next();
    if value then self.map(value)
  };
};

class TakeIterator : Iterator {
  op new(iter, num) = {
    self.iter = iter;
    self.max = num;
    self.cur = 0;
  };

  op next() = {
    if self.cur < self.max then {
      self.cur = self.cur + 1;
      self.iter.next()
    }
  };
};

pub class Range : Iterator {
  op new(from, to) = {
    self.from = from;
    self.to = to;
  };

  op next() = {
    if self.from < self.to then {
      let val = self.from;
      self.from = self.from + 1;
      val
    }
  };
};

class SkipIterator : Iterator {
  op new(iter, num) = {
    self.iter = iter;
    for i in Range(0, num) do {
      self.iter.next();
    };
  };

  op next() = self.iter.next();
};

class ZipIterator : Iterator {
  op new(a, b) = {
    self.a = a;
    self.b = b;
  };

  op next() = {
    let a = self.a.next();
    let b = self.b.next();
    if a != None && b != None then {
      [a, b]
    }
  };
};

pub class FunctionIterator : Iterator {
  op new(state, func) = {
    self.func = func;
    self.state = state;
  };

  op next() = self.func(self.state);
};