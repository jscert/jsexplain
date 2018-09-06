# Js\_of\_ocaml bis ("The Generator")
## Why bis? & Purpose
  Because, there is already a tool named `js_of_ocaml` whose job is
  to produce efficient Javascript from OCaml bytecode. We are not that tool.

  Here, we try to translate OCaml syntax to 'simple' ECMAScript syntax, the
  purpose of it is to generate readable ECMAScript code, so that it
  could later be use in a step-by-step ECMAScript interpreter.

## Dependencies
  See parent directory.

## What is built
  * **main**: The main ml-to-js compiler. Can produce files in one of 5 modes,
    using the `-mode` parameter:
    * `log`: Executable JS output with logging function calls for use with the
      debugger.
    * `unlog`: Plain executable JS output
    * `token`: Produces `unlog` annotated with token identifiers for `log`. And
      produces an `.mlloc.js` file mapping token identifiers to locations of the
      input `.ml` source file.
    * `pseudo`: 'Pseudo'-js output. Makes use of monadic let bindings alike the
      input OCaml to improve readability.
    * `ptoken`: `token` equivalent for `pseudo` output files.
  * **lineof**: Converts and combines `.token.js`, `.ptoken.js`, and `.mlloc.js`
    files into `lineof.js` which maps token identifiers to file locations.
  * **assembly**: Linker for compiled `.log.js` files into a single executable
    `.js` file.
  * **displayed_sources**: Produces string versions of the relevant source
    files, for display.
  * **monad_ppx**: PPX rewriter for monadic binder extension syntax. See below.

  Each of these programs may be compiled to a bytecode or native target as
  desired. Bytecode is the default.

### Makefile Targets
  The following `make` targets are defined:
  * `default`: Compiles bytecode versions of the programs and the stdlib.
  * `all`: Compiles bytecode and native versions of the programs and the stdlib.
  * `byte` / `native`: Compiles bytecode or native versions of the programs.
  * \*`.byte` / \*`.native`: Compiles the specified program into bytecode or
    native code.
  * `debug`: Compiles bytecode versions of the programs with debug symbols, and
    the appropriate `.ocamldebug` configuration file.
  * `test`: Compiles and executes the test suite. See below.
  * `clean`: Cleans built targets.

## How does it work?
  In order to get the statically typed abstract syntax tree (STAST) of OCaml we
  link against compiler-libs, we have tested against versions 4.04.0. Previous
  versions are available for >= 4.02.1.

  On top, of this STAST, there is a custom back-end that compiles OCaml to
  ECMAScript. The code written in OCaml cannot rely on code from the typical
  standard library. Therefore a file named `stdlib.mli` (found in the directory
  `stdlib_ml`) contains all the required definitions for the examples to work.
  This file as a twin which is `stdlib.js`, in this file the functions whose
  prototype is in `stdlib.mli` are defined here.

## About the subset of OCaml supported
  * **Let** declarations, except `let () =`. Recursive declarations are
    supported.
  * **If then else** statements, as expected `if then else` expressions return a
    value.
  * **Pattern matching**, only one level of pattern matching over arbitrary
    types.
  * **Type declarations**, if a constructor take arguments (one or more), you
    should add an annotations to provide default names for the parameters. See
    example below.
    ```ocaml
    type 'a tree = | Leaf of 'a [@f value] | Node of 'a
    tree * 'a * 'a tree [@f left, value, right]
    ```
  * **Records** Records are translated to objects. Record copying using the
    OCaml `with` syntax is supported for any number of field updates.

## About the Subset of JavaScript Used
  * Object.assign (ES6)
  * No type casting
  * No prototypes
  * Arrays (for tuples)
  * Switch on strings (used for type constructor matching)
  * `with` as a simplistic means of linking module definitions.

## About the Monadic Rewriter PPX
  This is a OCaml AST preprocessor that converts `let%x` syntax into the
  monadic binder `if_x` with the continuation as the bound expression of the
  let term.

  For example:
  * `let%some x = expr in cont` ↝ `if_some expr (fun x -> cont)`
  * `let%spec (s,x) = expr in cont` ↝ `if_spec expr (fun s x -> cont)`

  The full list of available monads is provided in the file
  [`monadic_binder_list.ml`](./monadic_binder_list.ml). This file should be
  configured as appropriate for the target application. The current
  configuration is for jsref.

## Test Suite
  The `tests` directory contains test files for the generator to compile. Test
  cases will first be built and run with the standard OCaml compiler to check
  that the assertions made within the tests are correct. The tests are then
  compiled with the generator and executed with Nodejs to ensure that the
  compiled versions are also correct. Cross-checking of results is not
  performed, it is expected that all tests should pass under both execution
  environments.

  The test case library interface is described in
  (tests/lib/mocha.mli)[./tests/lib/mocha.mli], it is designed to be similar in
  style to the Mocha JS testing framework. The JS test environment is backed
  directly by Mocha, the OCaml test environment is Alcotest with a wrapper to
  provide a Mocha interface.

  Tests are built and executed using the `make test` command in this directory.

## Other Notes
  Historical versions of the repository required the OCaml 4.02.1 compiler
  source code to compile directly against. We are unable to redistribute
  these files for licensing reasons. If for some reason you want to build a
  historic version, then drop the parsing, typing and utils directories from
  the OCaml distribution into this directory, some further configuration may
  be required... The whole historic distribution archived in the private
  jscert\_dev repository.
