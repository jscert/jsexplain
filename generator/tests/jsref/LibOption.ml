open LibReflect

(** val option_compare :
    'a1 coq_Comparable -> 'a1 option -> 'a1 option -> bool **)

let option_compare h o1 o2 =
  match o1 with
  | Some v1 ->
    (match o2 with
     | Some v2 -> h v1 v2
     | None -> false)
  | None ->
    (match o2 with
     | Some a -> false
     | None -> true)

(** val option_comparable :
    'a1 coq_Comparable -> 'a1 option coq_Comparable **)

let option_comparable h x y =
  option_compare h x y

(** val unsome_default : 'a1 -> 'a1 option -> 'a1 **)

let unsome_default d = function
| Some x -> x
| None -> d

(** val map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let map f = function
| Some x -> Some (f x)
| None -> None

