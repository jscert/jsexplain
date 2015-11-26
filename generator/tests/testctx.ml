
let testp1 x = 
  let (a,b,c) = x in
  a+b


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



let test00 x = 
  match x with
  | None -> 2
  | Some y -> y




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
    let y = 3 in y+1 
    in
  x


let test3 x =
  let y =
    match x with 
    | None -> 1
    | Some z -> z+2
    in
  y

let test4 x =
  match x with
  | None -> 1
  | Some y -> 
    begin match y with
    | None -> 2
    | Some z -> z 
    end


(*
let test4 a =
  let y = a+1 in
  let f x = x in
  f (let x = y in x)

let test5 x =
  match x with
  | None -> 0
  | Some x -> let x = x+x in x+1
*)

(*

let test6 = 
  function z -> match z with (x,y) -> x
*)
(*
let test6 (x,y) =
  x
  *)
  
  