# TODO

## Improve modules
* Trace over the entire module to look for declarations and build graph of
declarations. **DONE**
* Replace import statements with links to the fully qualified paths.
* Allow wildcard import
  ```
  import std::iter::_;
  ```
  Which will import all elements from that path. **DONE**
* Create prelude module with reexports to allow for:
  ```
  import std::prelude::_;
  ```
  Possibly implicitly include this in every file. **DONE**

## Improve parser
* 

