import std::iter::Range;

let INITIAL_CAPACITY = 10;

pub class HashMap {
  op new() = {
    self.buckets = [];
    for x in Range(0, INITIAL_CAPACITY) do {
      self.buckets.push([]);
    };
  };

  fn capacity() = self.buckets.len();

  fn get_bucket(value) = {
    let hash = value.hash();
    let bucket_num = hash % self.capacity();
    let bucket = self.buckets[bucket_num];
    bucket.push(value);
  };

  fn resize() = {

  };
};