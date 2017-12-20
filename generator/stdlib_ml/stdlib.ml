(** OCaml Compatibility Wrapper for the Js_of_ocaml bis Standard Library

This implementation of the library permits programs designed to be compiled
with the limited standard library of Js_of_ocaml bis to be compiled against the
OCaml standard library for native/bytecode targets.

This file is to be compiled with the standard OCaml compiler with the
-nopervasives flag enabled. It should be linked with the OCaml standard
library into the end program file.
*)
let raise = Pervasives.raise;;

(**{6 Boolean operations }*)
(** Note: Both OCaml and JS implement lazy evaluation for boolean operators. *)
let not = Pervasives.not;;
external ( && ) : bool -> bool -> bool = "%sequand";;
external ( || ) : bool -> bool -> bool = "%sequor";;

(**{6 Debugging }*)
external __LOC__ : string = "%loc_LOC"

(**{6 Integer arithmetic }*)
let ( + ) = Pervasives.( + );;
let ( - ) = Pervasives.( - );;
let ( * ) = Pervasives.( * );;
let ( / ) = Pervasives.( / );;

(**{6 Floating-point arithmetic }*)
let ( +. ) = Pervasives.( +. );;
let ( -. ) = Pervasives.( -. );;
let ( *. ) = Pervasives.( *. );;
let ( /. ) = Pervasives.( /. );;

(*
let ( ** ) = Pervasives.( ** );;
let atan = Pervasives.atan;;
let exp = Pervasives.exp;;
let log = Pervasives.log;;
let mod_float = Pervasives.mod_float;;
let float_of_int = Pervasives.float_of_int;;
let infinity = Pervasives.infinity;;
let neg_infinity = Pervasives.neg_infinity;;
let nan = Pervasives.nan;;
let max_float = Pervasives.max_float;;
let min_float = Pervasives.min_float;;
*)

(**{6 String operations }*)
(*
let (^) = Pervasives.(^);;
*)

(**{6 Character operations }*)
(*
let int_of_char = Pervasives.int_of_char;;
*)

(**{6 String conversion functions }*)
(*
let string_of_int = Pervasives.string_of_int;;
let string_of_float = Pervasives.string_of_float;;
let float_of_string = Pervasives.float_of_string;;
*)

(**{6 Input/output }*)
(*
let print_endline = Pervasives.print_endline;;
let prerr_string = Pervasives.prerr_string;;
let prerr_endline = Pervasives.prerr_endline;;
let prerr_newline = Pervasives.prerr_newline;;
*)

(**{6 References }*)
(** for future use for the global heap
let ref = Pervasives.ref;;
let (:=) = Pervasives.(:=);;
let (!) = Pervasives.(!);;
*)

(**{5 Pervasives-incompatible Definitions }

Functions in this section either deviate from the OCaml Pervasives type
signature, or are additional functions to fill in holes left by making a
polymorphic function not monomorphic.
*)

(**{6 Comparisons }*)
(** The standard comparison operators have been restricted to floating-point
operations only. *)
let ( = ) = Pervasives.( = );;
let ( < ) = Pervasives.( < );;
let ( > ) = Pervasives.( > );;
let ( <= ) = Pervasives.( <= );;
let ( >= ) = Pervasives.( >= );;

(*
let compare = Pervasives.compare;;
let min = Pervasives.min;;
let max = Pervasives.max;;
*)

let ( === ) = Pervasives.( = );;

(*
let float_compare = Pervasives.compare;;
*)
let int_eq = Pervasives.(=);;
let int_lt = Pervasives.(<);;
let int_gt = Pervasives.(>);;
let int_le = Pervasives.(<=);;
let int_ge = Pervasives.(>=);;
let int_compare = Pervasives.compare;;
let bool_eq = Pervasives.(=);;
let nat_eq = Pervasives.(=);;

let string_eq = Pervasives.(=);;
let string_lt = Pervasives.(<);;
let string_compare = Pervasives.compare;;

(**{6 Integer arithmetic }*)
(*let int_abs = Pervasives.abs;;*)

(**{6 Floating-point arithmetic }*)
(*
let fmod = Pervasives.mod_float;;
let float_neg = Pervasives.(~-.);;
let float_exp = Pervasives.exp;;
*)

(* Alan: Ideally we would add these to the spec, but for the moment conversion
   to a string is doing a foo+"", and conversion to an int is doing +foo *)
let int_of_number = Pervasives.int_of_float;;
let number_of_int = Pervasives.float_of_int;;

(**{6 String Operations }*)
(** String concatenation renamed from ^ *)
let strappend = Pervasives.(^);;

(**{5 String library }*)
(** Operations here are present in the OCaml standard library's String module
However cannot be used directly due to a potential module name collision when
linking to the standard library. (TODO: Work out if this can be avoided) *)

(*
This has the wrong typesignature for String.concat!
val string_concat : string -> string -> string (* + *)
*)

let strlength = String.length;;
let substring n m s = String.sub s n m;;

let console_int = Pervasives.print_int;;
