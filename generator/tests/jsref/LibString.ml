open LibReflect
open String0

(** val string_comparable : string coq_Comparable **)

let string_comparable x y =
  comparable_of_dec string_dec x y

