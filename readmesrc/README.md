# ULC

An interpreter for heavily sugared untyped lambda calculus.

<BADGES>

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

    <FILE:examples/example04_lists_map.ulc>

### Generating this README.md

This README.md has been generated automatically from the source in
`readmesrc`, do not edit manually.

To generate new readme, enter `sbt` console and run 
`clean`, `coverageOn`, `test`, `coverageReport`, `generateReadme`.

To look at it locally, you might also want to run something like

    markdown README.md > readme.html && firefox readme.html