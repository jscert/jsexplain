(* `make shadow.unlog.js` and test resulting function in node
   add a print_int to the shadower line and `ocamlc shadow.ml` to validate ml *)

let shadower n =
  let f _ =
    let n = n+1 in
    let n = n+1 in
    n+1 in
  f () ;;

shadower 1
(* Expected return value: 4 *)
