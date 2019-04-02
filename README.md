# ULC

An interpreter for heavily sugared untyped lambda calculus.

 ![version: 1.2](https://img.shields.io/static/v1.svg?label=version&message=1.2&color=lightgrey) ![local-build: passing](https://img.shields.io/static/v1.svg?label=local-build&message=passing&color=green) ![local-tests: passing](https://img.shields.io/static/v1.svg?label=local-tests&message=passing&color=green) ![statement-coverage: 70.73%](https://img.shields.io/static/v1.svg?label=statement-coverage&message=70.73%&color=yellow)  ![branch-coverage: 43.75%](https://img.shields.io/static/v1.svg?label=branch-coverage&message=43.75%&color=orange)

  *Tested locally on x86_64 GNU/Linux
 with `scalaVersion = 2.12.5`, `sbtVersion = 1.1.4`. Readme generated on 2019-04-02.* 

### Features

This little demo shows how Boolean algebra, 
natural numbers and arithmetic, mutually recursive function definitions
and simple data structures like lists 
can be expressed in *raw* untyped lambda calculus.

The input script is "compiled" down to a single lambda term.
There are absolutely no optimizations: booleans, pairs, integers, lists
are all represented by horrendously long lambda terms.
The performance is just good enough to demonstrate that some basic
algorithms work on tiny lists with small integers.

### Basic usage

The single-file script `ulc.scala` in `src/main/scala` 
is all you need to run simple
programs written in the sugared lambda calculus.

To run it, you need a Scala interpreter.
Here is the structure of the command line for basic usage 
in non-interactive mode:

    scala ulc.scala --type=<TYPE> <INPUT_FILE>

where `<TYPE>` is the type of the expression 
(required for human-readable output, see below),
and `<INPUT_FILE>` is the path to the input file with the code.

For example, in a Unix-like environment, when you are in the root 
directory of this project

    scala src/main/scala/ulc.scala \
      --type=List[Boolean] \
      examples/example04_lists_map.ulc

would run the code in `example04_lists_map.ulc`, and in the end
interpret the resulting term as a list of booleans, and print the list
in human readable format.

You can also start it in interactive mode:

    scala ulc.scala --interactive

and then experiment with smaller lambda expressions, e.g.:

    Enter lambda expression: (\f.\x.(f x)) (\y.y) z
    z

Note that it does not really feel like an interactive shell,
e.g. the Arrow-Up key does not let you edit the last command.
Line editors like 
[`ledit`](http://manpages.ubuntu.com/manpages/trusty/man1/ledit.1.html)
make the experience a little less painful:

    ledit scala ulc.scala --interactive

### Input Syntax

Basic lambda expressions are constructed as follows:

  * `(\varName.body)` - abstraction
  * `(function argument)` - application
  * `x` - variable name

For example:

    \f.(\x.f(x x))(\z.f(z z))

is an [fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus).

There is syntactic sugar for

  * Definition-like redexes (separated by `;` from the rest):

        val x = 42;
        // rest of the file, can use `x` from now on

    for both `val` and `def` keywords.

  * Function definitions:

        def S x y z = x z (y z);

  * Recursive functions (no explicit `Y`-combinators necessary):
 
        def fact n = if (isZero(n)) { 1 } else { mul n (fact(pred(n))) };
        // restOfCode

  * Mutually recursive functions:

        mutuallyRecursive {
          def odd n = if (isZero(n)) { false } else { even(pred(n)) } ,
          def even n = if (isZero(n)) { true } else { odd(pred(n)) }
        };

  * Natural numbers: `0`, `42`, `1000` etc. No negative integers, though.

  * Lists:

        [1, 2, 3]


The entire file is one single lambda term (it does not end with a semicolon).

### Output types

This interpreter compiles everything into raw lambda calculus - there
are no built-in numbers or lists, or anything at all, only lambda functions.
Therefore, even the simplest programs tend to evaluate to monstrously 
looking lambda-terms, which are unreadable for normal human beings.

In order to be able to interpret the output, we have to specify the expected
type of the output, and then the interpreter will interpret the resulting
term accordingly and pretty-print it in readable format.

Following types / type constructors are supported:

  * `Boolean`
  * `Int`
  * `List[A]` for types `A`
  * `(A, B)` for types `A`, `B`

The type of the expected output is specified using the `--type=` option.

### Example code

    // SKI-combinators.
    def S x y z = x z(y z);
    def K x y = x;
    def I x = x;
    
    // Basic boolean operations
    def true a b = a;
    def false a b = b;
    def or a b = a true b;
    def and a b = a b false;
    def not b = b false true;
    def if cond t else e = cond t e;
    
    // pair constructor together with deconstructors.
    def pair x y t = t x y;
    def fst pair = pair true;
    def snd pair = pair false;
    
    // natural numbers in Church encoding
    // def zero f x = x;
    // def succ n f x = f(n f x);
    // def isZero n = n (x.false) true;
    // def pred n f x = (
    //   def helper valueMarker = (
    //     val value = fst valueMarker;
    //     val marker = snd valueMarker;
    //     val newValue = if (marker) { f(value) } else { value };
    //     pair newValue true
    //   );
    //   val wrappedResult = n helper (pair x false);
    //   fst wrappedResult
    // );
    // def add a b = a succ b;
    // def sub fromWhat whatToSubtract = whatToSubtract pred fromWhat;
    // def mul a b = a (add b) zero;
    
    // Y-combinator, manually
    // def yHalf y f = f (y y f);
    // val Y = yHalf yHalf;
    // val Y = f.(x.f(x x))(z.f(z z));
    
    // natural numbers in list encoding
    val zero = pair true true;
    def succ n = pair false n;
    val one = succ(zero);
    def isZero n = fst n;
    def pred n = snd n;
    
    // applies function `f` to `x` `n` times,
    // desugared into a term with `Y`-combinator (it's recursive!)
    def repeat n f x = if (isZero(n)) { x } else { 
      f(repeat (pred n) f x) 
    };
    
    // Arithmetic operations in terms of `repeat`
    def add a b = repeat a succ b;
    def sub a b = repeat b pred a;
    def mul a b = repeat a (add b) zero; 
    def square  x = mul x x;
    def pow a b = repeat b (mul a) one;
    
    // non-trivial arithmetic operations that require recursion
    def factorial n = if (isZero(n)) { one } else { 
      mul n (factorial(pred(n))) 
    };
    
    // simple demo of mutually recursive functions
    mutuallyRecursive {
      def odd n = if (isZero(n)) { false } else { even(pred(n)) } ,
      def even n = if (isZero(n)) { true } else { odd(pred(n)) }
    };
    
    // more complex demo of mutually recursive functions:
    // Hofstadter sequences
    mutuallyRecursive {
      def female n = if (isZero(n)) { 1 } else { sub n (male(female(pred(n)))) },
      def male n = if (isZero(n)) { 0 } else { sub n (female(male(pred(n)))) }
    };
    
    // equality check for integers 
    def isEqual a b = if (and (isZero(a)) (isZero(b))) {
      true 
    } else (if (or (isZero(a)) (isZero(b))) {
      false 
    } else {
      isEqual (pred(a)) (pred(b))
    });
    
    // List data structure
    def listStructure empty h t = pair empty (pair h t);
    def nil = listStructure true I I;
    def cons h t = listStructure false h t;
    def isEmpty a = fst a;
    def head a = fst(snd a);
    def tail a = snd(snd a);
    
    // Operations on lists
    def foldLeft list start f = if (isEmpty(list)) { 
      start 
    } else {
      foldLeft (tail(list)) (f start (head(list))) f
    };
    
    def map list f = if (isEmpty(list)) {
      nil
    } else {
      cons (f(head(list))) (map (tail(list)) f)
    };
    
    // Generates list of integers {from, ..., to}
    // works only if `from <= to`
    def range from to = if (isEqual from to) { 
      cons to nil 
    } else { 
      cons from (range (succ(from)) to) 
    };
    
    // This is the term that is evaluated, (eventually interpreted) and printed 
    (
      // generate range of integers, test whether they are even or not
      map (range 0 2) even
    )
    

### Generating this README.md

This README.md has been generated automatically from the source in
`readmesrc`, do not edit manually.

To generate new readme, enter `sbt` console and run 
`clean`, `coverageOn`, `test`, `coverageReport`, `generateReadme`.

To look at it locally, you might also want to run something like

    markdown README.md > readme.html && firefox readme.html