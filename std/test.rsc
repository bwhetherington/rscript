import std::prelude::_;

pub fn try_get(list, index) = 
  if 0 <= index && index < list.len()
  then Some(list[index])
  else Nothing;

fn fizzbuzz() = {
  for x in Range(1, 101) do {
    let line = "";
    if x % 3 == 0 then {
      line = line + "Fizz";
    };
    if x % 5 == 0 then {
      line = line + "Buzz";
    };
    if line.len() == 0 then {
      line = "" + x;
    };
    println(line);
  };
};