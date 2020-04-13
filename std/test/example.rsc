import std::io::println;
import std::io::input;
import std::rand::rng;

pub class Program {
  op new(min, max) = {
    let real_number = rng().ints(min, max + 1).next();
    self.actual = real_number;
    self.is_running = False;
    self.guesses = 10;
  };

  fn start() = {
    self.is_running = True;
    self.run();
  };

  fn stop() = {
    self.is_running = False;
  };

  fn run() = {
    println("Guess a number between 1 and 100");
    while self.is_running && self.guesses > 0 do {
      let user_input = input("Guess: ").trim();
      if user_input == "stop" then {
        self.stop();
      } else {
        let guess = __eval__(user_input);
        if __type_of__(guess) == "Number" then {
          # Compare number to actual
          if guess == self.actual then {
            println("Congratulations, you win!");
            self.stop();
          } else if guess > self.actual then {
            println("Too high.");
            self.guesses = self.guesses - 1;
          } else {
            println("Too low.");
            self.guesses = self.guesses - 1;
          };
        } else {
          println("Please guess a number.");
        };
      };
    };
  };
};

pub fn play_game(min, max) = {
  let game = Program(min, max);
  game.start();
};