pub fn main() = {
  let xs = [1, 2];
  xs.push(10);
  xs.push(True);
  xs.push(None);

  let f = xs.get;
  print(f(2));
};