open Mocha

type shadow =
| Shadow of int [@f num]

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

  it "variable rebinding" (fun _ ->
    let x = 10 in
    let x = x + x in
    let x = x + x + x in
    assert_int x 60 "x should be able to be redeclared based upon the previous value of x"
  )
)
