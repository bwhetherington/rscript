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

  fn zip_with(iter, f) = ZipIterator(self, iter).map(fn(pair) = f(pair[0], pair[1]));

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
  fn sum() = self.fold(0, fn(a, b) = a + b);

  # Consumes this iterator, performing the specified function on each element.
  fn for_each(func) = {
    for x in self do {
      func(x);
    };
  };

  # Consumes this iterator, producing `True` if all values of this iterator
  # satisfy the given predicate.
  fn all(pred) = {
    let res = True;
    for x in self do {
      if !pred(x) then {
        res = False;
        break;
      };
    };
    res
  };

  # Consumes this iterator, producing `True` if any value of this iterator
  # satisfies the given predicate.
  fn any(pred) = {
    let res = False;
    for x in self do {
      if pred(x) then {
        res = True;
        break;
      };
    };
    res
  };

  fn looped() = LoopIterator(self);
};

class PeekableIterator ext Iterator {
  op new(iter) = {
    self._iter = iter;
    self._queue = [];
  };

  op next() = 
    if self._queue.len() > 0 
    then self._queue.pop()
    else self._iter.next();

  fn peek() = {
    let value = self.next();
    if value != None then {
      self._queue.push(value);
    };
    value
  };
};

pub class IndexIterator ext Iterator {
  op new(list) = {
    self._list = list;
    self._end = list.len();
    self._index = 0;
  };

  op next() = {
    if self._index < self._end then {
      let value = self._list[self._index];
      self._index = self._index + 1;
      value
    } 
  };

  fn looped() = LoopIndexIterator(self._list, self._index);
};

fn wrapping_add(x, addend, modulo) = (x + addend) % modulo;

class LoopIndexIterator ext Iterator {
  op new(list, index) = {
    self._list = list;
    self._index = index;
  };

  op next() = {
    let value = self._list[self._index];
    self._index = wrapping_add(self._index, 1, self._list.len());
    value
  };
};

List.iter = fn() =
  IndexIterator(self);

List.map = fn(f) = self.iter().map(f).list();

List.filter = fn(f) = self.iter().filter(f).list();

String.iter = fn() =
  IndexIterator(self);

class FilterIterator ext Iterator {
  op new(iter, pred) = {
    self._iter = iter;
    self._pred = pred;
  };

  op next() = {
    let value = self._iter.next();
    if value then {
      if self._pred(value) then {
        value
      } else {
        self.next()
      }
    }
  };
};

class MapIterator ext Iterator {
  op new(iter, func) = {
    self._iter = iter;
    self._func = func;
  };

  op next() = {
    let value = self._iter.next();
    if value then self._func(value)
  };
};

class TakeIterator ext Iterator {
  op new(iter, num) = {
    self._iter = iter;
    self._max = num;
    self._cur = 0;
  };

  op next() = {
    if self._cur < self._max then {
      self._cur = self._cur + 1;
      self._iter.next()
    }
  };
};

class TakeWhileIterator ext Iterator {
  op new(iter, pred) = {
    self._iter = iter;
    self._pred = pred;
  };

  op next() = {
    let cur = self._iter.next();
    if self._pred(cur) then {
      cur
    } else {
      None
    }
  };
};

pub class Range ext Iterator {
  op new(from, to) = {
    self._from = from;
    self._to = to;
  };

  op next() = {
    if self._from < self._to then {
      let val = self._from;
      self._from = self._from + 1;
      val
    }
  };
};

class SkipIterator ext Iterator {
  op new(iter, num) = {
    self._iter = iter;
    for i in Range(0, num) do {
      self._iter.next();
    };
  };

  op next() = self._iter.next();
};

class ZipIterator ext Iterator {
  op new(a, b) = {
    self._a = a;
    self._b = b;
  };

  op next() = {
    let a = self._a.next();
    let b = self._b.next();
    if a != None && b != None then {
      [a, b]
    }
  };
};

class LoopIterator ext Iterator {
  op new(iter) = {
    self._iter = iter;
    self._stored = [];
    self._index = -1;
  };

  fn is_first_iter() = self._index == -1;

  op next() = {
    if self.is_first_iter() then {
      let val = self._iter.next();
      if val == None then {
        self._index = 0;
        self.next()
      } else {
        self._stored.push(val);
        val
      }
    } else {
      let val = self._stored[self._index];
      self._index = wrapping_add(self._index, 1, self._stored.len());
      val
    }
  };
};

pub class FunctionIterator ext Iterator {
  op new(state, func) = {
    self._func = func;
    self._state = state;
  };

  op next() = self._func(self._state);
};