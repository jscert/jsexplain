
(*--------------------*)
(* int operations *)

(* todo : factorize and clean up *)
(*
val ( ~+ ) : int -> int
val ( ~- ) : int -> int
*)

val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( / ) : int -> int -> int

(*val int_abs : int -> int*)

val nat_eq : int -> int -> bool (* nat_eq x y = int_eq x y  *)
val int_eq : int -> int -> bool
val int_lt : int -> int -> bool
val int_gt : int -> int -> bool
val int_le : int -> int -> bool
val int_ge : int -> int -> bool
(* val int_lt : int -> int -> bool*)
val int_compare : int -> int -> int

(*--------------------*)
(* float operations *)

(* Alan: these can be implemented directly, using Number.NaN,
   Number.POSITIVE_INFINITY, Number.NEGATIVE_INFINITY *)
(*
val nan : float
val infinity : float
val neg_infinity : float
*)

(* Alan: Do we need these ? If so, they are Number.MAX_VALUE and
   Number.MIN_VALUE

val max_float : float
val min_float : float

 *)


(* Alan: these should all be implemented along with the int operations as the JS
   ones. ** is Math.pow *)

(*
val ( ~+. ) : float -> float
val ( ~-. ) : float -> float
*)

val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float
(* val ( ** ) : float -> float -> float *)


(*
val mod_float : float -> float -> float (* Alan: % infix *)
*)

(* Alan: Why do we need these? If need be, they are all in Math *)

(*
val atan : float -> float
val exp : float -> float
val log : float -> float
val min : float -> float -> float
val max : float -> float -> float
*)


(* Alan: Ideally we would add these to the spec, but for the moment conversion
   to a string is doing a foo+"", and conversion to an int is doing +foo *)

val int_of_number : float -> int (* will be removed, since only used by substring *)
val number_of_int : int -> float  (* = fun x -> float_of_int x *)



(*val float_of_string : string -> float*)
(*val float_of_int : int -> float*)
(*
val int_of_char : char -> int
val string_of_float : float -> string
val string_of_int : int -> string
*)

(* no need to implement those in stdlib.js because JS has them already *)
val ( = ) : float -> float -> bool
val ( < ) : float -> float -> bool
val ( > ) : float -> float -> bool
val ( <= ) : float -> float -> bool 
val ( >= ) : float -> float -> bool
val fmod : float -> float -> float (*  mod_float, implemented as % operator in JS *)


(*val compare : 'a -> 'a -> int*)
(*val float_lt : float -> float -> bool
val float_le : float -> float -> bool*)
(*val float_compare : float -> float -> int*)
(* val float_neg : float -> float  ~-.*)
(* val float_exp : float -> float -> float (* ( ** ) *) *)


(*--------------------*)
(* bool operations *)

val bool_eq : bool -> bool -> bool (* should be "===" in  JS *)
val ( && ) : bool -> bool -> bool  (* beware of strict vs lazy semantics: todo discuss --> just map to  *)
val not : bool -> bool
val ( || ) : bool -> bool -> bool  (* beware of strict vs lazy semantics: todo discuss --> just map to  *)


(*--------------------*)
(* string operations *)

(* todo : factorize and clean up *)

val string_eq : string -> string -> bool (* === *)
val string_compare : string -> string -> int

(*
   val string_concat : string -> string -> string (* + *)
   val (^) : string -> string -> string
*)

(* let append s1 s2 = String.append s1 s2 *)
val strappend : string -> string -> string  (* + *)

(* let strlength s = String.length s *)
val strlength : string -> int (* in JS :  function (x) { return x.length; } *)

(* let substring n m s = String.sub s n m *)
val substring : int -> int -> string -> string  (* function(x) { return x.slice(n, n+m); } *)
 (* only need to implement this when m=1 *)


(*--------------------*)
(* special operations *)

(* We use this to compare types that are not known by stdlib, like Native_error;
  should be implemented in JS by comparing the objects, to see if they have the same
  "tag" fields (there should be no other fields, except perhaps "type") *)
val ( === ) : 'a -> 'a -> bool  (* becomes === in js *)



(*--------------------*)
(* JSRef specific functions, useful for debugging *)

(* val print_endline : string -> unit *)

val __LOC__ : string (* todo: will not be needed in the future *)

(*
val prerr_string : string -> unit
val prerr_newline : unit -> unit
val prerr_endline : string -> unit
*)

val raise : exn -> 'a   (* bind to  throw "Not_found" *)
(*val stuck : string -> 'a*)





(*--------------------*)
(* for future use for the global heap

val ref : 'a -> 'a ref
val (:=) : 'a ref -> 'a -> unit
val (!) : 'a ref -> 'a
 *)

