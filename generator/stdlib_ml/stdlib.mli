(*
Field name attributes for builtins (eg: ::) are defined in attributes.ml
*)

val ( ~+ ) : int -> int
val ( ~- ) : int -> int
val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( / ) : int -> int -> int

type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
val nan : float
val infinity : float
val neg_infinity : float
val max_float : float
val min_float : float
val ( ~+. ) : float -> float
val ( ~-. ) : float -> float
val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float
val ( ** ) : float -> float -> float
val abs_float : float -> float
val mod_float : float -> float -> float
val atan : float -> float
val exp : float -> float
val log : float -> float
val floor : float -> float
val min : float -> float -> float
val max : float -> float -> float
val classify_float : float -> fpclass

val int_abs : int -> int

val float_of_int : int -> float
val float_of_string : string -> float
val int_of_char : char -> int
val int_of_float : float -> int
val string_of_float : float -> string
val string_of_int : int -> string

(* We use this to compare types that are not known by stdlib, like Native_error *)
val ( === ) : 'a -> 'a -> bool
(*val ( <> ) : 'a -> 'a -> bool*)
(*val ( < ) : 'a -> 'a -> bool
val ( > ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool 
val ( >= ) : 'a -> 'a -> bool
*)

(* no need to implement those in stdlib.js because JS has them already *)
val ( = ) : float -> float -> bool
val ( < ) : float -> float -> bool
val ( > ) : float -> float -> bool
val ( <= ) : float -> float -> bool 
val ( >= ) : float -> float -> bool

(*val compare : 'a -> 'a -> int*)

val bool_eq : bool -> bool -> bool
val int_eq : int -> int -> bool
val int_lt : int -> int -> bool
val int_ge : int -> int -> bool
val string_eq : string -> string -> bool
val float_eq : float -> float -> bool
val float_lt : float -> float -> bool
val float_le : float -> float -> bool
val float_compare : float -> float -> int

val string_concat : string -> string -> string


val pi : float
val e : float
val ln2 : float

val float_neg : float -> float (* ~-.*)

val lt_bool : float -> float -> bool (*   (<) *)
val add : float -> float -> float (*  (+.) *)
val sub : float -> float -> float (*  (-.) *)
val mult : float -> float -> float (*  ( *. ) *)
val div : float -> float -> float (*  (/.) *)
val fmod : float -> float -> float (*  mod_float *)
val float_exp : float -> float -> float (* ( ** ) *)



val ( && ) : bool -> bool -> bool
val not : bool -> bool
val stuck : string -> 'a

(* Structural equality, need to be careful with implementation 
val (=) : 'a -> 'a -> bool*)

val (^) : string -> string -> string

val ref : 'a -> 'a ref
val (:=) : 'a ref -> 'a -> unit
val (!) : 'a ref -> 'a


(* no longer needed it seems *)
module Pervasives : sig
  val succ : int -> int
end

(* should not be needed if we do'nt use JsNumber.ml *)
module Int32 : sig
  val logand : int32 -> int32 -> int32
  val lognot : int32 -> int32
  val logor : int32 -> int32 -> int32
  val logxor : int32 -> int32 -> int32
  val of_float : float -> int32
  val shift_left : int32 -> int -> int32
  val shift_right : int32 -> int -> int32
  val shift_right_logical : int32 -> int -> int32
  val to_float : int32 -> float
end

(* should not be needed if we do'nt use JsNumber.ml *)
module Int64 : sig 
  val one : int64
  val float_of_bits : int64 -> float
end

module List : sig (* should rely on List0 instead *)
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev : 'a list -> 'a list
end

module String : sig (* should rely on String0 instead *)
  val length : string -> int
  val append : string -> string -> string
  val sub : string -> int -> int -> string
  val concat : string -> string list -> string
  val iter : (char -> unit) -> string -> unit
  val make : int -> char -> string
  val get : string -> int -> char
end

(* Coq outputs exceptions in the place of arbitrary *)
val raise : exn -> 'a

(* JSRef specific functions *)
val prerr_string : string -> unit
val prerr_newline : unit -> unit
val prerr_endline : string -> unit


module Parser_syntax : sig (* needed by translate_syntax.mli and by parser_main (below) *)
  type unary_op
  type arith_op
  type bin_op
  type exp
end

module Parser_main : sig 
  val exp_from_string : ?force_strict:bool -> string -> Parser_syntax.exp
end 

(* ARTHUR: not needed
module Parser : sig
  exception ParserFailure of string
  exception InvalidArgument
end

*)

module Obj : sig (* should not be needed *)
  type t
end


(* only used for debug*)

val print_endline : string -> unit
val __LOC__ : string
