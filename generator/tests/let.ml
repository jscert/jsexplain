let test0 x =
  let y = 2*x in
  y+y


let foo x =
  let abr = 1
  and bli = 4 in
  if true then abr else bli

let app x = x

let app2 = fun x -> x

type carte =
  | As
  | Petite of int [@f petite] 


let affiche x = match x with
  | As -> "As"
  | Petite n -> "Petite"

let pet = Petite 5
let cinq = 5

let test b = match b with
  | true -> ()
  | false -> ()

let test x = match x with
  | 1 -> ()
  | 2 -> ()
  | _ -> ()
