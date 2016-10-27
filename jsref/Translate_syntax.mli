(* parse_esprima strictness source *)
val parse_js_syntax : bool -> string -> JsSyntax.prog option

(* Not implemented in JS *)
(* potentially throws an exception *)
val parse_js_syntax_from_file : bool -> string -> JsSyntax.prog

exception CoqSyntaxDoesNotSupport of string
