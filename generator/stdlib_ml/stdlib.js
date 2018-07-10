// JS implementations for OCaml's Pervasives library
// See stdlib.mli for documentation

// Infix operators are converted to equivalent JS in
//   js_of_ast.ml#js_of_path_longident

//----------------------------------------------------------------------------
// Js_of_ocaml bis Generator Requirements

// type option 'a = None | Some of 'a
var None = function() {
   return { /*type: "option",*/ tag: "None" };
};

var Some = function(value) {
   return { /*type: "option",*/ tag: "Some", value: value };
};

// type list 'a = [] | :: of 'a * list 'a
var mk_nil = function() {
   return { /*type: "list",*/ tag: "[]" };
};

var mk_cons = function(head, tail) {
   return { /*type: "list",*/ tag: "::", head: head, tail: tail };
};

//----------------------------------------------------------------------------
// Exceptions

// val raise : exn -> 'a
var raise = function(x) { throw x; };
var failwith = function(str) { throw Error(str); };

//----------------------------------------------------------------------------
// Boolean operations

// val not : bool -> bool
var not = function(x) { return !x; };

// val ( && ) : bool -> bool -> bool
// val ( || ) : bool -> bool -> bool
// Operators mapped directly to JS equivalent

//----------------------------------------------------------------------------
// Deugging

// val __LOC__ : string
var __LOC__ = "___LOC___"

//----------------------------------------------------------------------------
// Integer Arithmetic

// val ( + ) : int -> int -> int
// val ( - ) : int -> int -> int
// val ( * ) : int -> int -> int
// val ( / ) : int -> int -> int
// Operators mapped directly to JS equivalent

//----------------------------------------------------------------------------
// Floating-point Arithmetic

// val ( +. ) : float -> float -> float
// val ( -. ) : float -> float -> float
// val ( *. ) : float -> float -> float
// val ( /. ) : float -> float -> float
// Operators mapped to JS equivalent by removing .

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

// Pervasives-incompatible functions

// Infix operators are converted to equivalent JS in
//   js_of_ast.ml#js_of_path_longident

//----------------------------------------------------------------------------
// Comparisons

// val ( = ) : float -> float -> bool
// Mapped to ==

// val ( < ) : float -> float -> bool
// val ( > ) : float -> float -> bool
// val ( <= ) : float -> float -> bool
// val ( >= ) : float -> float -> bool
// Operators mapped directly to JS equivalent

/*
// val compare : 'a -> 'a -> int
// val min : float -> float -> float
// val max : float -> float -> float
*/

// val ( === ) : 'a -> 'a -> bool

/*
// val float_compare : float -> float -> int
*/

// val int_eq : int -> int -> bool
var int_eq = function(x, y) { return x === y; };

// val int_lt : int -> int -> bool
var int_lt = function(x, y) { return x < y; };

// val int_gt : int -> int -> bool
var int_gt = function(x, y) { return x > y; };

// val int_le : int -> int -> bool
var int_le = function(x, y) { return x <= y; };

// val int_ge : int -> int -> bool
var int_ge = function(x, y) { return x >= y; };

// val int_compare : int -> int -> int
var int_compare = function(x, y) { return x - y; };

// val bool_eq : bool -> bool -> bool
var bool_eq = function(x, y) { return x === y; };

// val nat_eq : int -> int -> bool (* nat_eq x y = int_eq x y  *)
var nat_eq = function(x, y) { return x === y; };

// val string_eq : string -> string -> bool (* === *)
var string_eq = function(x, y) {
  if (typeof(x) != "string" || typeof(y) != "string")
    throw "string_eq invalid arguments";
  return x === y;
};

// val string_lt : string -> string -> bool
var string_lt = function(x, y) {
  if (typeof(x) != "string" || typeof(y) != "string")
    throw "string_lt invalid arguments";
  return x < y;
};

// val string_compare : string -> string -> int
var string_compare = function(x, y) {
  return x.localeCompare(y);
};

//----------------------------------------------------------------------------
// Floating-point arithmetic

/* Functions renamed:
// val fmod : float -> float -> float (*  mod_float, implemented as % operator in JS *)
// val float_neg : float -> float          (* ~-. *)
// val float_exp : float -> float -> float (* exp *)
*/

// Alan: Ideally we would add these to the spec, but for the moment conversion
// to a string is doing a foo+"", and conversion to an int is doing +foo
// val int_of_number : float -> int (* will be removed, since only used by substring *)
var int_of_number = function(x) { return x; };

// val number_of_int : int -> float  (* = fun x -> float_of_int x *)
var number_of_int = function(x) { return x; };

//----------------------------------------------------------------------------
// String operations

// val string_concat : string -> string -> string (* + *)

// val strappend : string -> string -> string
var strappend = function(x, y) {
  if (typeof(x) != "string" || typeof(y) != "string")
    throw "strappend invalid arguments";
  return x + y;
};

// val strlength : string -> int
var strlength = function(x) {
  if (typeof(x) != "string")
    throw "strlength invalid arguments";
  return x.length;
};

// val substring : int -> int -> string -> string
var substring = function(n, m, s) {
  if (typeof(s) != "string")
    throw "strlength invalid arguments";
  return s.slice(n, n+m);
};
