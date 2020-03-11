import std::prelude::_;
import std::math::_;

pub fn main() = {
  let map = HashMap();
  map["key1"] = 10;
  map["key2"] = 20;
  for entry in map.entries() do {
    println(entry.key + ": " + entry.value);
  };
};