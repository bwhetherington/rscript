pub class Iterator {
  # Produces the next element of the iterator, if present.
  op next() = None;

  # Produces a new iterator that produces only elements from this iterator that
  # meet the specified condition.
  fn filter(pred) = FilterIterator(self, pred);

  # Produces a new iterator that produces elements from this iterator that are
  # passed through the specified function.
  fn map(f) = MapIterator(self, f);

  # Produces a new iterator that produces only the first `count` elements from
  # this iterator.
  fn take(count) = TakeIterator(self, count);

  fn take_while(pred) = TakeWhileIterator(self, pred);

  # Produces a new iterator that produces only elements after the first `count`
  # elements from this iterator.
  fn skip(count) = SkipIterator(self, count);

  # Produces a new iterator that produces pairs of elements from this iterator
  # and the other specified iterator.
  fn zip(iter) = ZipIterator(self, iter);

  fn zip_with(iter, f) = ZipIterator(self, iter).map(|pair| f(pair[0], pair[1]));

  # Produces itself.
  fn iter() = self;

  # Produces the elements from this iterator in the range [from, to).
  fn slice(from, to) = self.skip(from).take(to - from);

  fn peekable() = PeekableIterator(self);

  # Consumes this iterator, producing a list containing all of its elements.
  fn list() = {
    let list = [];
    for value in self do {
      list.push(value);
    };
    list
  };

  # Consumes this iterator, producing the result of a right fold across it.
  fn fold(acc, func) = {
    for x in self do {
      acc = func(acc, x);
    };
    acc
  };

  # Consumes this iterator, producing the result of a right fold across it,
  # starting with its first element as the initial value.
  fn reduce(func) = {
    let acc = self.next();
    self.fold(acc, func)
  };

  # Consumes this iterator, producing the sum of its elements
  fn sum() = self.fold(0, |a, b| a + b);

  # Consumes this iterator, performing the specified function on each element.
  fn for_each(func) = {
    for x in self do {
      func(x);
    };
  };

  op plus(other) = self.zip_with(other, |a, b| a + b);
};

class PeekableIterator ext Iterator {
  op new(iter) = {
    self.iter = iter;
    self.queue = [];
  };

  op next() = 
    if self.queue.len() > 0 
    then self.queue.pop()
    else self.iter.next();

  fn peek() = {
    let value = self.next();
    if value != None then {
      self.queue.push(value);
    };
    value
  };
};

class IndexIterator ext Iterator {
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
};

List.iter = || {
  IndexIterator(self)
};

String.iter = || {
  IndexIterator(self)
};

class FilterIterator ext Iterator {
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

class MapIterator ext Iterator {
  op new(iter, map) = {
    self.iter = iter;
    self.map = map;
  };

  op next() = {
    let value = self.iter.next();
    if value then self.map(value)
  };
};

class TakeIterator ext Iterator {
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

class TakeWhileIterator ext Iterator {
  op new(iter, pred) = {
    self.iter = iter;
    self.pred = pred;
  };

  op next() = {
    let cur = self.iter.next();
    if self.pred(cur) then {
      cur
    } else {
      None
    }
  };
};

pub class Range ext Iterator {
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

class SkipIterator ext Iterator {
  op new(iter, num) = {
    self.iter = iter;
    for i in Range(0, num) do {
      self.iter.next();
    };
  };

  op next() = self.iter.next();
};

class ZipIterator ext Iterator {
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

pub class FunctionIterator ext Iterator {
  op new(state, func) = {
    self.func = func;
    self.state = state;
  };

  op next() = self.func(self.state);
};