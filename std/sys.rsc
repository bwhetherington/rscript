# Binding for native `__exit__` function.
pub fn exit(code) = __exit__(code);

# Binding for native `__argv__` function.
pub fn args() = __argv__().iter();

# Binding for native `__unix_time__` function.
pub fn time() = __unix_time__();