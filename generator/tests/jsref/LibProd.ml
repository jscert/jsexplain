(** val prod_compare :
     ('a1 -> 'a1 -> bool)  ->  ('a2 -> 'a2 -> bool)  -> ('a1 * 'a2) -> ('a1 * 'a2) ->
    bool **)

let prod_compare h h0 x y =
  let (x1, x2) = x in let (y1, y2) = y in (h x1 y1) && (h0 x2 y2)
