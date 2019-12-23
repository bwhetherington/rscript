fn last(xs) = xs[xs.len() - 1];

fn first(xs) = xs[0];

fn push_all(to, from) = {
  let i = 0;

  let len = from.len();
  while i < len {
    let val = from[i];
    to.push(val);
    i = i + 1;
  };
};

pub fn main() = {
  let xs = [1, 2, 3];
  print(xs.to_string());
  print(first(xs), last(xs));
};