(** val string_dec : char list -> char list -> bool **)

let string_dec s1 s2 =
  let rec f = function
  | [] ->
    (fun s0 ->
      match s0 with
      | [] -> true
      | a::s3 -> false)
  | a::s0 ->
    (fun s3 ->
      match s3 with
      | [] -> false
      | a0::s4 -> if (=) a a0 then if f s0 s4 then true else false else false)
  in f s1 s2

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val length : char list -> int **)

let rec length = function
| [] -> 0
| c::s' -> Pervasives.succ (length s')

(** val substring : int -> int -> char list -> char list **)

let rec substring n m s =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      [])
      (fun m' ->
      match s with
      | [] -> s
      | c::s' -> c::(substring 0 m' s'))
      m)
    (fun n' ->
    match s with
    | [] -> s
    | c::s' -> substring n' m s')
    n

