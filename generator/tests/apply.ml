open Mocha

let foo y = ()

let x = foo ()

;;

describe "apply.ml" (fun _ ->
  it "foo ()" (fun _ ->
    assert_unit x
  )
)
