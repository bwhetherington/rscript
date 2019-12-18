import foo::foo;

fn add(x, y) = x + y;

pub fn main() = {
  let sum = add(4, add(9, 3));
  console.log(foo::id(sum));
};