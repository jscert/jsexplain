val string_of_char_list : string -> string
val prvalue : JsSyntax.value -> string
val prstate : bool (* skip init *) -> JsSyntax.state -> string
val prref : JsSyntax.ref -> string
val prresvalue : JsSyntax.resvalue -> string
val prloc : JsSyntax.object_loc -> string
