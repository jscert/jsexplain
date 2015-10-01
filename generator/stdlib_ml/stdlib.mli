(*
Field name attributes for builtins (eg: ::) are defined in attributes.ml
*)

(* Custom pair type *)
type ('a, 'b) pair =
| Pair [@f fst, snd] of 'a * 'b

val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( / ) : int -> int -> int

val ( === ) : 'a -> 'a -> bool
val ( < ) : 'a -> 'a -> bool
val ( > ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool 
val ( >= ) : 'a -> 'a -> bool

(* Structural equality, need to be careful with implementation *)
val (=) : 'a -> 'a -> bool

(* Coq extraction builtins refer directly to Pervasives at times *)
module Pervasives : sig
  val succ : int -> int
end

(* Coq outputs exceptions in the place of arbitrary *)
val raise : exn -> 'a

val print : 'a -> unit

val stuck : string -> 'a 
val to_string : 'a -> string
val parse : 'a -> 'b
