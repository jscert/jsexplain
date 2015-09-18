open LibReflect

(** val prod_compare :
    'a1 coq_Comparable -> 'a2 coq_Comparable -> ('a1 * 'a2) -> ('a1 * 'a2) ->
    bool **)

let prod_compare h h0 x y =
  let (x1, x2) = x in let (y1, y2) = y in and_decidable (h x1 y1) (h0 x2 y2)

(** val prod_comparable :
    'a1 coq_Comparable -> 'a2 coq_Comparable -> ('a1 * 'a2) coq_Comparable **)

let prod_comparable cA cB x y =
  prod_compare cA cB x y

