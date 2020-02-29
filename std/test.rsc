import std::monad::_;

pub fn try_get(list, index) = 
  if 0 <= index && index < list.len()
  then Some(list[index])
  else Nothing;

import std::prelude::_;
pub let p = std::prelude::println;