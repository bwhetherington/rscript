pub let Ok = Object();
pub let Err = Object();

Ok.new = |ok| {
  self.value = ok;
};

Ok.map = |f| Ok(f(self.value));

Ok.flat_map = |f| f(self.value);

Ok.to_string = || "Ok(" + self.value + ")";

Err.new = |err| {
  self.err = err;
};

Err.map = |f| self;

Err.flat_map = |f| self;

Err.to_string = || "Err(" + self.err + ")";

pub let Some = Object();

Some.new = |value| {
  self.value = value;
};

Some.map = |f| Some(f(self.value));

Some.flat_map = |f| f(self.value);

Some.to_string = || "Some(" + self.value + ")";

pub let Nothing = Object();

Nothing.map = |f| self;

Nothing.flat_map = |f| self;

Nothing.to_string = || "Nothing";