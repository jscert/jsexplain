(** val plus : int -> int -> int **)

let rec plus = (+)

(** val nat_iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let rec nat_iter n f x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    x)
    (fun n' ->
    f (nat_iter n' f x))
    n

