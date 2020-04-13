import std::prelude::_;

let INITIAL_CAPACITY = 2;
let RATIO = 2 / 3;

pub let hash = __hash__;

pub class Entry {
  op new(key, value) = {
    self.key = key;
    self.value = value;
  };

  op to_string() = "{" + self.key + ": " + self.value + "}";
};

pub class HashMap {
  op new() = {
    self._buckets = List.with_size(fn() = [], INITIAL_CAPACITY);
    self._size_internal = 0;
  };

  fn size() = self._size_internal;

  fn capacity() = self._buckets.len();

  fn get_bucket_index(key) = hash(key) % self.capacity();

  fn get_bucket(key) = self._buckets[self.get_bucket_index(key)];

  fn get_entry(key) = {
    let bucket = self.get_bucket(key);
    let found = None;
    for entry in bucket.iter() do {
      if entry.key == key then {
        found = entry;
        break;
      };
    };
    found
  };

  fn get_entry_or_insert(key) = {
    let bucket = self.get_bucket(key);
    let found = None;
    for entry in bucket.iter() do {
      if entry.key == key then {
        found = entry;
        break;
      };
    };

    # If we didn't find an entry, insert a new one
    if found == None then {
      let entry = Entry(key, None);
      bucket.push(entry);
      self._size_internal = self._size_internal + 1;
      found = entry;
    };

    found
  };

  fn entry_list() = {
    let entries = [];
    for bucket in self._buckets.iter() do {
      for entry in bucket.iter() do {
        if entry.value != None then {
          entries.push(entry);
        };
      };
    };
    entries
  };

  fn entries() = {
    self.entry_list().iter()
  };

  fn keys() = self.entries().map(fn(entry) = entry.key);

  fn values() = self.entries().map(fn(entry) = entry.value);

  fn resize() = {
    # Store existing entries
    let existing = self.entry_list();
    let capacity = self.capacity() * 1.5;
    self._buckets = List.with_size(fn() = [], capacity);
    for entry in existing do {
      self.insert(entry.key, entry.value);
    };
  };

  fn insert(key, value) = {
    if (self.size() + 1) / self.capacity() >= RATIO then {
      self.resize();
    };
    let entry = self.get_entry_or_insert(key);
    entry.value = value;
  };

  fn get(key) = {
    Option
      .from(self.get_entry(key))
      .map(fn(entry) = entry.value)
      .unwrap()
  };

  fn remove(key) = {
    let entry = self.get_entry(key);
    if entry != None then {
      if entry.value != None then {
        # This is the only case where we actually remove a value
        entry.value = None;
        self._size_internal = self._size_internal - 1;
        True
      } else {
        False
      }
    } else {
      False
    }
  };

  op index_set(index, value) = {
    self.insert(index, value);
  };

  op index_get(index) = self.get(index);

  fn contains_key(key) = {
    let entry = self.get_entry(key);
    if entry != None 
      then entry.value != None 
      else False
  };

  op to_string() = {
    let s = "{";
    let inner = ", ".join(self.entries().map(fn(entry) = entry.key + ": " + entry.value).list());
    s + inner + "}"
  };
};

pub class HashSet {
  op new() = {
    self._map = HashMap();
  };

  fn insert(el) = {
    self._map[el] = True;
  };

  fn remove(el) = {
    self._map.remove(el);
  };

  fn contains(el) = self._map.contains_key(el);

  fn iter() = self._map
    .entries()
    .map(fn(entry) = entry.key);

  op to_string() = {
    let s = "{";
    let inner = ", ".join(self.iter().list());
    s + inner + "}"
  };
};