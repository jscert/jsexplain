open Bool0
open LibBool

type coq_Decidable =
  bool
  (* singleton inductive, whose constructor was decidable_make *)

(** val true_decidable : coq_Decidable **)

let true_decidable =
  true

(** val false_decidable : coq_Decidable **)

let false_decidable =
  false

(** val bool_decidable : bool -> coq_Decidable **)

let bool_decidable b =
  b

(** val not_decidable : coq_Decidable -> coq_Decidable **)

let not_decidable h =
  neg h

(** val or_decidable : coq_Decidable -> coq_Decidable -> coq_Decidable **)

let or_decidable h h0 =
  coq_or h h0

(** val and_decidable : coq_Decidable -> coq_Decidable -> coq_Decidable **)

let and_decidable h h0 =
  coq_and h h0

type 'a coq_Comparable =
  'a -> 'a -> coq_Decidable
  (* singleton inductive, whose constructor was make_comparable *)

(** val comparable_of_dec : ('a1 -> 'a1 -> bool) -> 'a1 coq_Comparable **)

let comparable_of_dec h x y =
  if h x y then true else false

(** val bool_comparable : bool coq_Comparable **)

let bool_comparable x y =
  eqb x y

(** val prop_eq_decidable :
    coq_Decidable -> coq_Decidable -> coq_Decidable **)

let prop_eq_decidable x y = bool_eq x y

