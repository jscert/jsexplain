
let testa x = 
  x

let testb x = 
  let a = x in a

let testc x = 
  let y = x in 
  let x = y+1 in
  x+y

let testd x =
  let f x = 
    let y = x in y in
  f x

let teste x =
  let y =
    let z = x in z in
  y

let testf x =
  let f a = 
    let y = a in x+y in
  f x


let testg x =
  if x then 1 else 0





let test0 f = 
  match Some 3 with
  | None -> 2
  | Some y -> y


let test1 on = 
  let x = 
    match on with 
    | None -> 0
    | Some n -> n
    in
  2*x

let test2 v =
  let x = 
    let x = 3 in x+1 
    in
  x


let test3 x =
  let x =
    match x with 
    | None -> 1
    | Some x -> x+2
    in
  x

let test4 x =
  let y = x+1 in
  let x x = x in
  x (let x = y in x)

let test5 x =
  match x with
  | None -> 0
  | Some x -> let x = x+x in x+1

(*

let test6 = 
  function z -> match z with (x,y) -> x
*)
(*
let test6 (x,y) =
  x
  *)
  
  