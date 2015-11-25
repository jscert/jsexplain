open LibReflect
open String0

(** val string_comparable : char list coq_Comparable **)

let string_comparable x y =
  comparable_of_dec string_dec x y

