import std::hash::HashMap;

pub fn memoize(f) = {
  let results = HashMap();
  |x| {
    let prev = results[x];
    if prev then prev else {
      let val = f(x);
      results[x] = val;
      val
    }
  }
};