type container =
  | Alpha [@f value] of int
  | Beta [@f]

let empty = Beta

let create_container value = Alpha value
let get_value cont = match cont with
	| Alpha x -> x
	| _ -> 0