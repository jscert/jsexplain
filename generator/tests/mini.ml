open Mocha

type toto =
  Foo of int [@f foo]

let test f = match f with
  Foo x -> x

;;

describe "mini.ml" (fun _ ->
  it "test" (fun _ ->
    assert_int (test (Foo 5)) 5 "test (Foo 5) === 5"
  )
)
