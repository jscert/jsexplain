(** {4 Js_of_ocaml bis Standard Library }

{5 Pervasives-compatible Definitions }

All functions in this section have the same type definitions as in OCaml's
Pervasives. The semantics of mathematical operations may vary as, for now, the
JS implementation will use floating-point based arithmetic.

Definitions will proceed in the order of the OCaml Pervasives interface.
*)

(**{6 Js_of_ocaml bis Generator Requirements }*)
(** The generator inserts calls to these functions/constructors directly into
the generated JS code. *)

(**{7 OCaml Syntax Helpers }*)
(** The syntax [{ einit with lbl = exp }] is implemented in JS as a call to the
JS function [record_with(einit, lbl, exp)] *)

(**{7 OCaml Built-in Types }*)
(** The following typedefs are built into the OCaml compiler, so are not
required in this interface, however, they are included in the JS
implementation of this library as the functions None, Some, mk_nil, mk_cons.
[
  type option 'a = None | Some of 'a
  type list 'a = [] | :: of 'a * list 'a
]
*)

(**{6 Exceptions }*)
(** Behaves as [throw "Not_found"] in JS. *)
val raise : exn -> 'a

(**{6 Boolean operations }*)
(** Note: Both OCaml and JS implement lazy evaluation for boolean operators. *)
val not : bool -> bool
val ( && ) : bool -> bool -> bool
val ( || ) : bool -> bool -> bool

(**{6 Debugging }*)
val __LOC__ : string

(**{6 Integer arithmetic }*)
val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( / ) : int -> int -> int

(**{6 Floating-point arithmetic }*)
val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float

(*
val ( ** ) : float -> float -> float
val atan : float -> float
val exp : float -> float
val log : float -> float
val mod_float : float -> float -> float (* Alan: % infix *)
val float_of_int : int -> float
val infinity : float
val neg_infinity : float
val nan : float
val max_float : float
val min_float : float
*)

(**{6 String operations }*)
(*
val (^) : string -> string -> string
*)

(**{6 Character operations }*)
(*
val int_of_char : char -> int
*)

(**{6 String conversion functions }*)
(*
val string_of_int : int -> string
val string_of_float : float -> string
val float_of_string : string -> float
*)

(**{6 Input/output }*)
(*
val print_endline : string -> unit
val prerr_string : string -> unit
val prerr_endline : string -> unit
val prerr_newline : unit -> unit
*)

(**{6 References }*)
(** for future use for the global heap
val ref : 'a -> 'a ref
val (:=) : 'a ref -> 'a -> unit
val (!) : 'a ref -> 'a
*)

(**{5 Pervasives-incompatible Definitions }

Functions in this section either deviate from the OCaml Pervasives type
signature, or are additional functions to fill in holes left by making a
polymorphic function not monomorphic.
*)

(**{6 Comparisons }*)
(** The standard comparison operators have been restricted to floating-point
operations only. *)
val ( = ) : float -> float -> bool
val ( < ) : float -> float -> bool
val ( > ) : float -> float -> bool
val ( <= ) : float -> float -> bool
val ( >= ) : float -> float -> bool

(*
val compare : 'a -> 'a -> int
val min : float -> float -> float
val max : float -> float -> float
*)

(** This function is treated specially by the Generator. For 'known' types, it
compiles directly down to the === operator in JS. For unknown types, it
converts it into a function call to _compare_TYPENAME. Known type are listed
in js_of_ast.ml#is_triple_equal_type (currently int, bool, string).

Precise semantics of this operator are awkward. It is not implemented by
default for floats due to incosnsitencies of requirements about the equiality
of +0 and -0, and NaN values. JSRef has a custom implementation making +0=≠=-0
and NaN===NaN *)
val ( === ) : 'a -> 'a -> bool

(*
val float_compare : float -> float -> int
*)
val int_eq : int -> int -> bool
val int_lt : int -> int -> bool
val int_gt : int -> int -> bool
val int_le : int -> int -> bool
val int_ge : int -> int -> bool
val int_compare : int -> int -> int
val bool_eq : bool -> bool -> bool
val nat_eq : int -> int -> bool
val string_eq : string -> string -> bool
val string_compare : string -> string -> int

(**{6 Integer arithmetic }*)
(* Function renamed from abs *)
(*val int_abs : int -> int*)

(**{6 Floating-point arithmetic }*)
(* Functions renamed:
val fmod : float -> float -> float (*  mod_float, implemented as % operator in JS *)
val float_neg : float -> float          (* ~-. *)
val float_exp : float -> float -> float (* exp *)
*)

(* Alan: Ideally we would add these to the spec, but for the moment conversion
   to a string is doing a foo+"", and conversion to an int is doing +foo *)
val int_of_number : float -> int (* will be removed, since only used by substring *)
val number_of_int : int -> float  (* = fun x -> float_of_int x *)

(**{6 String Operations }*)
(** strappend is a renamed form of (^) *)
val strappend : string -> string -> string

(**{5 String library }*)
(** Operations here are present in the OCaml standard library's String module
However cannot be used directly due to a potential module name collision when
linking to the standard library. (TODO: Work out if this can be avoided) *)

(*
Conflicts with String.concat definition? Wrong type signature!
val string_concat : string -> string -> string (* + *)
*)

val strlength : string -> int (* in JS :  function (x) { return x.length; } *)

(** Substring extraction. Note different ordering of arguments from String.sub:
[ substring n m s = String.sub s n m ] *)
val substring : int -> int -> string -> string  (* function(x) { return x.slice(n, n+m); } *)
