open Syntax

let bar s = Js.Unsafe.fun_call (Js.Unsafe.js_expr "Syntax.Bar") [|Js.Unsafe.inject s|]
let baz i = Js.Unsafe.fun_call (Js.Unsafe.js_expr "Syntax.Bar") [|Js.Unsafe.inject i|]

let convProg = function
  | Bar s -> bar s
  | Baz s -> baz s
