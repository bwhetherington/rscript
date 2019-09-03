pub func add(x: Int, y: Int): Int = {
  x + y
};

pub func fibonacci(x: Int): Int = {
  if x < 2 then {
    x
  } else {
    fibonacci(x - 1) + fibonacci(x - 2)
  }
};