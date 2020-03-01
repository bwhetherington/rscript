pub class Result {};

pub class Ok ext Result {
  op new(value) = {
    self.value = value;
  };

  fn map(f) = Ok(f(self.value));

  fn flat_map(f) = f(self.value);

  op to_string() = "Ok(" + self.value + ")";

  fn unwrap() = self.value;
};

pub class Err ext Result {
  op new(err) = {
    self.err = err;
  };
  
  fn map(_) = self;

  fn flat_map(_) = self;

  op to_string() = "Err(" + self.err + ")";
};

pub class Option {
  fn from(value) = 
    if value != None
    then Some(value)
    else Nothing;
};

pub class Some ext Option {
  op new(value) = {
    self.value = value;
  };

  fn map(f) = Some(f(self.value));

  fn flat_map(f) = f(self.value);

  op to_string() = "Some(" + self.value + ")";

  fn unwrap() = self.value;
};

pub class Nothing ext Option {
  fn map(_) = self;
  fn flat_map(_) = self;
  op to_string() = "Nothing";
  fn unwrap() = None;
};