# Programming Languages Midterm

## Part 1

**1.**

```lisp
(define (raise-num [x : number]) : number
  (cond [(> x 0) 0]
        [else x]))

(define (raise [xs : (listof number)]) : (listof number)
  (map raise-num xs))
```

**2.**

```lisp
(define (alternating2-helper [xs : (listof 'a)] [skip-first : boolean]) : (listof 'a)
  (cond [skip-first (alternating2-helper (rest xs))]
        [else (cons (first xs) (alternating2-helper (rest xs) (not skip-first)))]))

(define (alternating2 [xs : (listof 'a)]) : (listof 'a)
  (alternating2-helper xs #f))
```

**3.**

```lisp
(define (any [pred : ('a -> boolean)] [xs : (listof 'a)]) : boolean
  (cond [(empty? xs) #f]
        [else (or (pred (first xs)) (any pred (rest xs)))]))

(define (sum [xs : (listof number)]) : number
  (foldr + 0 xs))

(define (avg [xs : (listof number)]) : number
  (let ([len (length xs)])
    (cond [(= len 0) -1]
          [else (/ (sum xs) len)])))

;; Finds the sublist following the first element of the given list to satisfy the given predicate
(define (find-start [pred : ('a -> boolean)] [xs : (listof 'a)]) : (listof 'a)
  (cond [(empty? xs) xs] ;; List is empty, nothing to find
        [else (cond [(pred (first xs)) (rest xs)]
                    [else (find-start pred (rest xs))])]))

(define (rainfall2 [xs : (listof number)]) : number
  (let ([to-avg (cond [(any (lambda [x] (= x -999)) xs) (find-start (lambda [x] (not (= x -999))) xs)]
                      [else xs])])
    (avg (filter (lambda [x] (> x 0)) to-avg))))
```

**4.**

```lisp
(define-type Equation
  ;; Represents an equation of the form: a * x^2 + b * x + c = 0
  (quadratic [a : number] [b : number] [c : number]))
```

**5.**

```lisp
(define (solve a b c)
  (let ([disc (- (* b b) (* 4 a c))])
    (cond [(< disc 0) (error 'solve "discriminant < 0")]
          [else (list
                 (/ (+ (- b) (sqrt disc)) (* 2 a))
                 (/ (- (- b) (sqrt disc)) (* 2 a)))])))
```

## Part 2

**1.**

- `(t1)` => `1`
- `(t2)` => `2`
- `(h1)` => `1`
- `(h2)` => `1`
- `(f)` => `1`
- `(f)` => `2`

**2.**

- **a.** `0`
- **b.** `0`
- **c.** `0`
- **d.** `99`

## Part 3

**A.**

If `extend-env` were instead implemented to append the new binding to the end of the environment, rather than to prepend it, the behavior of `lookup` in its original state wouldn't be entirely the same. Since new bindings are inserted at the beginning of the list, more recently inserted bindings will be found when calling `lookup`. I do not think this would warrant any changes to `lookup`. To maintain exactly the same behavior, `lookup` would have to iterate backward through the list of bindings, rather than forwards.

**B.**

`fetch` would likewise have to iterate backwards over the list to ensure that the most recent binding is found first.

**C.**

Broadly speaking, imperative programming centers around the idea of giving a machine instructions to execute. Functional programming, however, centers around constructing expressions to be evaluated, similarly to how one would evaluate an expression in math. With imperative programming, the programmer tells a computer _how_ to compute some desired output, whereas with functional programming, the programmer describes _what_ should be computed.

**D.**

The expression below:

```lisp
(with ((x A)) B)
```

Is syntactic sugar for the following expression:

```lisp
((lambda (x) B) A)
```

**E.**

As demonstrated above, binding values to names can be accomplished purely through the use of lambda functions. Because of this, lambda functions are sufficient for providing a mechanism to name values. Because lambda functions are sufficient, syntactic sugar may be introduced to provide a cleaner syntax for naming values.

## Part 4

**A.**

Dynamic scoping means that when some code for a language is run, e.g. our interpreter evaluates an expression, the result is dependent on the current state of the machine. The values associated with variables, for instance, are not tied to where they are in the program, but simply what has been defined by the time any specific expression is evaluated. Static, or lexical scoping, provides a mechanism where the bindings in use at any one point are tied to the structure of a program itself. The following C code provides an example of this:

```c
int x = 10;
{
    int x = 20;
    // Within this scope, x now refers to a different variable
}
// x is still 10, as the binding now refers to the x in this scope
```

Lexical scoping in general provides behavior that is easier to reason about, because the structure of a program is more evident from simply looking at the code than the exact behavior would be with dynamic scoping. With lexical scoping, the programmer can clearly see where a new scope has been entered. To figure out how it will work with dynamic scoping, the programmer has to go through the code carefully to figure out what variables will have what values at any given time.

**B.**

The following program will not work properly under dynamic scoping.

```lisp
(define printvar (displayln x)) ;; An error will be thrown here, as under no circumstances will x have been defined yet. We can fix this by making printvar a function
(define (testfunc cond)
  (if cond
    (let ((x "Hello"))
      (printvar))
    (printvar))) ;; An error will be thrown here as well. If cond is false, then there will be no x variable defined.
```

The corrected form of this code under dynamic scoping should be as follows:

```lisp
(define (printvar) (displayln x))
(define (testfunc cond)
  (if cond
    (let ((x "Hello"))
      (printvar))
    (let ((x "Not hello"))
      (printvar))))
```

**C.**

The program will also not work under static scoping rules.

```lisp
(define printvar (displayln x)) ;; An error will be thrown here for the same reason as in the dynamically scoped version. In addition, even when it is made a function, x will not be guaranteed to be in scope, so we should take x as an argument to the function
(define (testfunc cond)
  (if cond
    (let ((x "Hello"))
      (printvar))
    (printvar)))
;; We will then want to provide an argument to the function in both cases
```

```lisp
(define (printvar x) (displayln x)) ;; Or (define printvar displayln)
(define (testfunc cond)
  (if cond
    (printvar "Hello")
    (printvar "Not hello")))
```

**D.**

Statically-scoped languages can support nested functions where the inner function may access variables defined by the outer function by utilizing closures. With closures, functions essentially contain a snapshot of the scope that they were created in. As an example, I have included some code from a language I am working on:

```
# Create a new function taking another function as input
pub fn memoize(f) = {
  # Create a map to store previously calculated results in
  let results = HashMap();
  # Create a new function to return
  fn(x) = {
    # When this closure is created, it scans through the code to find identifiers referenced in it
    # In this case, it sees that results is accessed, so it stored the current value of results at this scope
    let prev = results[x];
    if prev then prev else {
      let val = f(x);
      results[x] = val;
      val
    }
  }
};
```

The function produced by calling, as an example, `memoize(fibonacci)` stores the HashMap that is accessed within its body. When a closure is invoked, it pushes values from its captured environment, in this case the HashMap results, into the scope when it executes.

## Part 5

**A.**

With lazy evaluation, expressions are only evaluated when absolutely necessary, as opposed to in eager evaluation, where expressions are fully evaluated as soon as they are called. An advantage of lazy evaluation is that it avoids potentially evaluating values which are never used. A counterargument to the effectiveness of lazy evaluation is there there is a certain amount of overhead involved in being able to essentially pause and resume evaluation at any time.

**B.**

C uses eager evaluation, and Haskell quite famously uses lazy evaluation for everything. This allows for use cases such as this interesting definition of the Fibonacci sequence:

```haskell
fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
```

**C.**

The code will not terminate (or rather a stack overflow may occur) if evaluated with eager evaluation, as the recursive function has no base case.

**D.**

If evaluated lazily, it will produce `1`.

**E.**

Suspension allows the execution of a program to be halted and resumed at a later time. The state required for the execution is stored when it is suspended. One could use it for lazy evaluation by suspending execution when the needed output is produced.

## Part 6

**A.**

Calling `(myrecfunc empty n)` should produce a list containing the first `n` powers of 2, skipping 2^0, i.e. `(myrecfunc empty 3)` would produce `'(8 4 2)`.

**B.**

`myrecfunc` is referenced within its own definition. Nothing is bound to the `myrecfunc` identifier until the lambda is evaluated, and so when the lambda is evaluated, an error will be produced as it tries to evaluate the currently nonexistent `myrecfunc`.

**C.**

This can be fixed by simply replacing `let` with `letrec`.

**D.**

`letrec` works by replacing the bindings defined in `letrec` with `let` bindings that bind the identifiers to boxes containing dummy values. The body of the new `let` expression is then made using a `begin` expression with two steps. The first step uses `set-box!` to insert the actual values of the `letrec` bindings into the `let`-bound boxes. The values are set to the values defined in the original `letrec` expression, but with references to the recursive binding wrapped in `unbox`. The next step is the body of the original `letrec` expression.

## Part 7

**A.**

A `box` is a way of storing some value behind another value, akin to storing an item behind a pointer in a language like C. Specifically, if a language has some form of store for storing values, a `box` is a location pointing to a value within the store. This allows the value pointed to by the `box` to be changed without changing the value used by the `box` at every location it's used.

**B.**

Mutation based on variables and mutation based on boxes produces fundamentally different results. Suppose that one takes assigns a variable `a` to be equal to another variable `b`. Then, suppose that variable `b` is mutated. If we are mutating variables, then `a` will not have changed as a result of `b`'s change, as when it is assigned as equal to `b`, it is essentially creating a copy of `b` as it existed at that time. Suppose, however, that `b` was originally a `box`, and that `a` is now also a `box`. Since all that was copied as the location of that `box`, if the value within it is changed through a call of `(set-box! b 'foo)`, the value stored behind `a` and accessed with `(unbox a)` will reflect the change to `b`'s value.

**C.**

```lisp
(define (override-store [var : Storage] [store : Store]) : Store
  (cond [(empty? store) (list var)]
        [else (type-case Storage var
          (cell [new-loc new-val]
            (type-case Storage (first store)
              (cell [old-loc old-val]
                (cond [(= new-loc old-loc) (cons var store)]
                      [else (cons (first store) (override-store var (rest store)))])))))]))
```

**D.**

Mutable state can make certain computations considerably easier to express, but does introduce additional room for errors. Mutable state can almost be seen to be similar to dynamic scoping. Since the value of a computation depends on the state at any given time, it may not always produce the same result. With immutable state, however, a function called with the same inputs should always produce the same output. This can make it easier to reason about the behavior of a program. It can come with downsides for performance, though, if new values must be created rather than mutating existing ones.

## Part 8

**A.**

### Uncurried

```lisp
(define (tobecurried a b c)
  (if (> a 0)
      (* (first b) (second c))
      (+ (second b) (first c))))
```

### Curried

```lisp
(define tobecurried
  (lambda [a] (lambda [b] (lambda [c]
    (if (> a 0)
        (* (first b) (second c))
        (+ (second b) (first c)))))))
```

**B.**

No, the curried and uncurried versions cannot be invoked in the same way in Racket. Because explicit parentheses are required to invoke functions, invoking the curried version must be done like so:

```lisp
(((tobecurried a) b) c)
```

**C.**

Considering the curried function written in part A, if the user invokes the function like `(tobecurried a)`, the closure returned allows it to capture the parameter `a` that was passed to it. This means that a user can then trivially create a version of a function that already has some of its parameters supplied, without needing to supply those parameters again at every stage.

**D.**

I find that Haskell makes excellent use of currying, which does make a certain amount of sense, given that both the language and currying are named after the same person. In many languages, there are a number of higher ordered functions for operating on lists, or iterators, like `map`, `filter`, `fold`, etc. Many object-oriented languages which allow for methods allow users to compose these functions as a chain of method calls. Languages like Racket and Haskell do not allow for method calls as they exist in other languages, i.e. `foo.bar()`. Haskell provides another way of neatly composing these higher ordered functions, however. That approach is to make all of these functions which operate on lists take that list as their final parameter. By providing function parameters but not list parameters to these functions, and then composing them as written below, a user can more easily compose these functions together than in Racket.

```haskell
map (\x -> x * x) . filter (\x -> x `rem` 2 == 0) $ [1, 2, 3, 4]
```

In the above code, the composed function represents a function to square the even numbers in a list. Obviously this is a contrived example, but I hope it helps to illustrate the effectiveness of currying in function composition.
