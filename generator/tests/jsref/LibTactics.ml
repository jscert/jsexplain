(** val let_binding : 'a1 -> ('a1 -> 'a2) -> 'a2 **)

let let_binding v k =
  k v

