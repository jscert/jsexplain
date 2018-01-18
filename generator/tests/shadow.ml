(* `make shadow.unlog.js` and test resulting function in node
   add a print_int to the shadower line and `ocamlc shadow.ml` to validate ml *)

(** Inner-most scope should not hide outermost, incorrect JS behaviour would be to execute undefined + 1 **)
let shadower n =
  let f _ =
    let (n, z) = n+1, () in
    n+1 in
  f () ;;

shadower 1;;            (* Expected return value: 3 *)


(** As previous, but using a pattern binder instead of let **)

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

f () (* Expected return value: 3 *)
;;

(** Things we want to be able to do, for example: **)
let x = 10 in
let x = x + x in
let x = x + x + x in
x

;;
