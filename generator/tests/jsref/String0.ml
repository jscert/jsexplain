(** val string_dec : char list -> char list -> bool **)

let rec string_dec s1 s2 = match s1 with
  | [] -> (match s2 with
    | [] -> true
    | a::s3 -> false)
  | a::s0 -> (match s2 with
    | [] -> false
    | a0::s4 -> if (=) a a0 then string_dec s0 s4 else false)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val length : char list -> int **)

let rec length l = match l with
| [] -> 0
| c::s' -> Pervasives.succ (length s')

(** val substring : int -> int -> char list -> char list **)

let rec substring n m s =
  if n=0 then
    if m=0 then
      []
    else match s with
      | [] -> s
      | c::s' -> c::(substring 0 (m-1) s')
  else match s with
    | [] -> s
    | c::s' -> substring (n-1) m s'
