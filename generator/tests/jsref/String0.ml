

(*

(** val string_dec : string -> string -> bool **)

let rec string_dec s1 s2 = match s1 with
  | [] -> (match s2 with
    | [] -> true
    | a::s3 -> false)
  | a::s0 -> (match s2 with
    | [] -> false
    | a0::s4 -> if (=) a a0 then string_dec s0 s4 else false)

(** val append : string -> string -> string **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val length : string -> int **)

let rec length l = match l with
| [] -> 0
| c::s' -> 1 + (length s')

(** val substring : int -> int -> string -> string **)

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


*)