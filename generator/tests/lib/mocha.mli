(** Mocha-compatible shim for OCaml and js_of_ocaml_bis

    This shim is designed to provide a similar interface and test runner to the JavaScript mocha library
    (https://mochajs.org/)

    A set of test suite modules linked together with this module will run each together in sequence when executed as a
    main program. For example:

[
    describe "test_module_1" (fun _ ->
      describe "function_1" (fun _ ->
        it "should return 1" (fun _ ->
          assert_equal (function_1 ()) 1
        )
      )
    )
]

   Alcotest uses 3 levels to test hierarchy:
   * test_case (string * speed * fun), where fun consists of assertions
   * test (string * test_case list)
   * test suite (string * test list)

   For this shim, there will be one test suite with a constant name,
   each test file this module is compiled with will be a test.
   Each mocha test ("it" definitions) will be test_cases, with names
   generated from the describe calls.

   To compile with the stock OCaml compiler, link your test case files
   against this and the Alcotest library. This library will then discover
   and execute your test cases. For example:
     ocamlfind ocamlc -package alcotest -linkpkg -g -o tests -I lib lib/mocha.cmo test1.ml test2.ml test3.ml ...
   executing "tests" will execute all test cases.
   Note that compilation with debug symbols is required to get source file
   information for test case failures.

   To compile with our OCaml-JS backend, compile each test case as usual,
   link each to the Mocha shim individually, and then execute all using
   the mocha command line tool:
     ../generator.byte -mode unlog -I lib test1.ml
     ../assembly.byte -o test1.unlog.assembly.js lib/mocha.js test1.unlog.js
     mocha test1.unlog.assembly.js


    A full sample test case file:
[
open Mocha

let test _ = assert_ok true "pass!"

let _ = describe "suite 1" (fun _ ->
  it "test 1" test;
  it "test 2" test;
  describe "nest 1" (fun _ ->
    it "test3" test;
    describe "nest2" (fun _ ->
      it "test4" test
    );
    describe "nest3" (fun _ ->
      it "test5" test
    )
  )
)
]

*)


val describe : string -> (unit -> unit) -> unit
val it : string -> (unit -> unit) -> unit

val assert_ok : bool -> string -> unit
val assert_fail : string -> unit

val assert_bool : bool -> bool -> string -> unit
val assert_int : int -> int -> string -> unit
val assert_float : float -> float -> string -> unit
val assert_char : char -> char -> string -> unit
val assert_string : string -> string -> string -> unit

(** Assert that a value is unit. This doesn't make much sense in ml, but does for the js compile target *)
val assert_unit : unit -> unit
val assert_struct_eq : 'a -> 'a -> string -> unit

(** [assert_failwith f e m] Assert that [f] raises a Failure with string [e]. The assert is labelled with [m]. *)
val assert_failwith : (unit -> 'a) -> string -> string -> unit
