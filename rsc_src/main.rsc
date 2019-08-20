# func fibonacci(n: Int): Int = {
#   if n < 2 then {
#     n
#   } else {
#     let sum = fibonacci(n - 1) + fibonacci(n - 2);
#     sum
#   }
# };

pub func main() = {
  console.log(fibonacci(10));
};