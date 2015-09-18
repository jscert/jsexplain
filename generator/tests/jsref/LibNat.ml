open LibReflect

(** val nat_compare : int -> int -> bool **)

let rec nat_compare x y =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      true)
      (fun n ->
      false)
      y)
    (fun x' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      false)
      (fun y' ->
      nat_compare x' y')
      y)
    x

(** val nat_comparable : int coq_Comparable **)

let nat_comparable x y =
  nat_compare x y

