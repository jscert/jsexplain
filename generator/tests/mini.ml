type toto =
  Foo of int [@f foo]

let test f = match f with
  Foo x -> x
