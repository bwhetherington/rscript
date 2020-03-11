import std::prelude::_;

let INITIAL_CAPACITY = 10;
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
    self.buckets = [];
    self.size_internal = 0;
    for x in Range(0, INITIAL_CAPACITY) do {
      self.buckets.push([]);
    };
  };

  fn size() = self.size_internal;

  fn capacity() = self.buckets.len();

  fn get_bucket_index(key) = hash(key) % self.capacity();

  fn get_bucket(key) = self.buckets[self.get_bucket_index(key)];

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
      self.size_internal = self.size_internal + 1;
      found = entry;
    };

    found
  };

  fn entries() = {
    let entries = [];
    for bucket in self.buckets.iter() do {
      for entry in bucket.iter() do {
        if entry.value != None then {
          entries.push(entry);
        };
      };
    };
    entries.iter()
  };

  fn keys() = self.entries().map(|entry| entry.key);

  fn values() = self.entries().map(|entry| entry.value);

  fn resize() = {

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
      .map(|entry| entry.value)
      .unwrap()
  };

  fn remove(key) = {
    let entry = self.get_entry(key);
    if entry != None then {
      if entry.value != None then {
        # This is the only case where we actually remove a value
        entry.value = None;
        self.size_internal = self.size_internal - 1;
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
    let inner = String.from(self.entries().map(|entry| entry.key + ": " + entry.value).list(), ", ");
    s + inner + "}"
  };
};

pub class HashSet {
  op new() = {
    self.map = HashMap();
  };

  fn insert(el) = {
    self.map[el] = True;
  };

  fn remove(el) = {
    self.map.remove(el);
  };

  fn contains(el) = self.map.contains_key(el);

  fn iter() = self.map
    .entries()
    .map(|entry| entry.key);

  op to_string() = {
    let s = "{";
    let inner = String.from(self.iter().list(), ", ");
    s + inner + "}"
  };
};