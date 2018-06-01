open Mocha

let test x =
  let r = () in
  r

;;

describe "letno.ml" (fun _ ->
  it "test" (fun _ ->
    assert_unit (test 0);
    assert_unit (test 1)
  )
)
