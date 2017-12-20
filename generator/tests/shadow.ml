(* `make shadow.unlog.js` and test resulting function in node
   add a print_int to the shadower line and `ocamlc shadow.ml` to validate ml *)

let shadower n =
  let f _ =
    let n = n+1 in
    n+1 in
  f () ;;

shadower 1;;            (* Expected return value: 3 *)


type shadow =
| Shadow of int [@f num]

let shadower2 n =
  let f _ =
    match n with
    | Shadow n -> n+1
  in
  f ()
;;

shadower2 (Shadow 1)  (* Expected return value: 2 *)
;;

let f _ =
  let x = 1 in
  let y n =
    let x = x + n in
    x in
  y (y x)
;;

console_int (f ()) (* Expected return value: 3 *)
