open Syntax
open SyntaxConv

let do_parse s =
  Baz s

let _ =
  Js.export "myParseLib"
    (object%js
      method parse s f = convProg (do_parse s)
    end)
