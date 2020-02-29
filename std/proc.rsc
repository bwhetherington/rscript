# Binding for native `__exit__` function.
pub let exit = __exit__;

# Binding for native `__argv__` function.
pub fn args() = __argv__().iter();

# Binding for native `__unix_time__` function.
pub let time = __unix_time__;