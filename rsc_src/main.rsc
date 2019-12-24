pub fn roll(n) = {
  rand::DEFAULT_RNG
    .map(|num| floor(num * 20) + 1)
    .take(n)
    .collect()
};

fn count_occurrences =

pub fn main() = {
  print(roll(10));
};