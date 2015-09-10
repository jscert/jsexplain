open Datatypes

type coq_Decidable =
  bool
  (* singleton inductive, whose constructor was decidable_make *)

type 'a coq_Comparable =
  'a -> 'a -> coq_Decidable
  (* singleton inductive, whose constructor was make_comparable *)

