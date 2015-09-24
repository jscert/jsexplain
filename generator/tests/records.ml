type pers =
  { name : string
  ; status : string
  ; age : int
  }

let boss =
  { name = "smith"
  ; status = "boss"
  ; age = 48
  }

let ab = boss.age

(* We do not support with syntax
 * let newboss = { boss with name = "john" } *)

(* Field punning *)
let newboss =
  let name = "pun" in
  let status = "awful" in
  let age = 0 in
  { name; status; age }
