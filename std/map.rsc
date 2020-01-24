# import std::iter::Range;

# let BASE_CAPACITY = 10;

# let Pair = Object();

# Pair.new = |a, b| {
#   self.key = a;
#   self.value = b;
# };

# pub let HashMap = Object();

# HashMap.new = || {
#   self.buckets = [[]] * BASE_CAPACITY;
# };

# HashMap.grow = || {
#   # Store the old values
#   let old_buckets = self.buckets;

#   # Double capacity
#   self.buckets = [[]] * self.buckets.len() * 2;

# };

# HashMap.get_bucket = |key| {
#   let bucket = key.hash() % self.buckets.len();
#   self.buckets[bucket]
# };

# HashMap.insert = |key, value| {
#   let pair = Pair(key, value);
#   let bucket = self.get_bucket(key);
#   bucket.push(pair);
# };

# HashMap.lookup = |key| {
#   let bucket = self.get_bucket(key);
#   let found = None;
#   for pair in bucket.iter() {
#     if pair.key == key then {
#       found = pair.value;
#       break;
#     };
#   };
#   found
# };