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

val float_of_int : int -> float
val float_of_string : string -> float
val int_of_char : char -> int
val int_of_float : float -> int
val string_of_float : float -> string
val string_of_int : int -> string

val ( < ) : 'a -> 'a -> bool
val ( > ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool 
val ( >= ) : 'a -> 'a -> bool
val compare : 'a -> 'a -> int

(* Structural equality, need to be careful with implementation *)
val (=) : 'a -> 'a -> bool

val (^) : string -> string -> string

val ref : 'a -> 'a ref
val (:=) : 'a ref -> 'a -> unit
val (!) : 'a ref -> 'a

(* Coq extraction builtins refer directly to Pervasives at times *)
module Pervasives : sig
  val succ : int -> int
end

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

module Int64 : sig
  val one : int64
  val float_of_bits : int64 -> float
end

(* This may be awkward! *)
module Lazy : sig
  type 'a t
end

module List : sig
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev : 'a list -> 'a list
end

module String : sig
  val concat : string -> string list -> string
  val iter : (char -> unit) -> string -> unit
  val make : int -> char -> string
end

(* Coq outputs exceptions in the place of arbitrary *)
val raise : exn -> 'a

(* JSRef specific functions *)
val prerr_string : string -> unit
val prerr_newline : unit -> unit
