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

let newboss = { boss with name = "john" }

let newboss = { boss with name = "john"; status = "newboss" }

(* Field punning *)
let newboss =
  let name = "pun" in
  let status = "awful" in
  let age = 0 in
  { name; status; age }
