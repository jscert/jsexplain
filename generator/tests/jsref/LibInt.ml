open BinInt
open Datatypes
open LibReflect

(** val my_Z_of_nat : int -> float **)

let my_Z_of_nat =
  Z.of_nat

(** val comparison_compare : comparison -> comparison -> bool **)

let comparison_compare c1 c2 =
  match c1 with
  | Eq ->
    (match c2 with
     | Eq -> true
     | Lt -> false
     | Gt -> false)
  | Lt ->
    (match c2 with
     | Eq -> false
     | Lt -> true
     | Gt -> false)
  | Gt ->
    (match c2 with
     | Eq -> false
     | Lt -> false
     | Gt -> true)

(** val comparison_comparable : comparison coq_Comparable **)

let comparison_comparable x y =
  comparison_compare x y

(** val int_comparable : float coq_Comparable **)

let int_comparable x y =
  comparison_comparable (Z.compare x y) Eq

