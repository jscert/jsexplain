open Mocha
open Shadow_include

type shadow =
| Shadow of int [@f num]

type ('t, 'a) if_ret_type =
| Return of 't [@f result]
| Continue of 'a [@f cont]

let let_ret w k =
  match w with
  | Continue s -> k s
  | Return  r -> r

;;

describe "shadow.ml" (fun _ ->
  it "shadower" (fun _ ->
    let shadower n =
      let f _ =
        let (n, z) = n+1, () in
        n+1 in
      f () in
    assert_int (shadower 1) 3 "Inner-most scope should not hide outermost, incorrect JS behaviour would be to execute undefined + 1"
  );

  it "shadower2" (fun _ ->

    let shadower2 n =
      let f _ =
        match n with
        | Shadow n -> n+1
      in
      f () in
    assert_int (shadower2 (Shadow 1)) 2 "Inner-most scope should not hide outermost, incorrect JS behaviour would be to execute undefined + 1"
  );

  it "shadower3" (fun _ ->
    let shadower3 _ =
      let x = 1 in
      let y n =
        let x = x + n in
        x in
      y (y x)
    in
    assert_int (shadower3 ()) 3 "shadower3 failed?"
  );

  it "monadic_tuple_shadow" (fun _ ->
    let monadic_tuple_shadow x s base =
      let%ret (s, base) = Continue x
      in (s, base)
    in
    assert_struct_eq (monadic_tuple_shadow (1, 2) 0 0) (1, 2) "Tuple rebinding not working."
  );

  it "variable rebinding" (fun _ ->
    let x = 10 in
    let x = x + x in
    let x = x + x + x in
    assert_int x 60 "x should be able to be redeclared based upon the previous value of x"
  );

  it "recursively shadows" (fun _ ->
    let myrec x = 0 in
    let rec myrec y = if y then 1 else myrec true in
    assert_int (myrec false) 1 "recursive function should shadow"
  )[@ocaml.warning "-26"];

  it "shadows across modules" (fun _ ->
    let external_def c = 0 in
    assert_int (external_def ()) 0 "external_def should have been shadowed"
  )
)
