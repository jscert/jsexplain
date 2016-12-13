let monad_mapping = [
    ("string", "if_string");
    ("object", "if_object");
    ("value", "if_value");
    ("prim", "if_prim");
    ("number", "if_number");
    ("some", "if_some");
    ("bool", "if_bool");
    ("void", "if_void");
    ("success", "if_success");
    ("not_throw", "if_not_throw");
    ("ter", "if_ter");
    ("break", "if_break");
    ("spec", "if_spec");
    ("assert", "check_assert");
    ("can_return", "if_spec_else_return");
   ]

let monad_identifiers = List.map (fun (_, f) -> "JsInterpreterMonads." ^ f) monad_mapping
