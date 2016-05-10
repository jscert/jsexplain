function record_with(einit, lbl, exp) {
  var res = {};
  for (var i in einit) {
    res[i] = einit[i];
  }
  res[lbl] = exp;
  return res;
}

//----------------------------------------------------------------------------

var None = function() {
   return { /*type: "option",*/ tag: "None" };
};

var Some = function(value) {
   return { /*type: "option",*/ tag: "Some", value: value };
};

//----------------------------------------------------------------------------

var mk_nil = function() {
   return { /*type: "list",*/ tag: "[]" };
};

var mk_cons = function(head, tail) {
   return { /*type: "list",*/ tag: "::", head: head, tail: tail };
};

//----------------------------------------------------------------------------

// var print = function (x) { console.log(x) }

// var stuck = function (msg) { throw {type:'stuck', msg:msg} }

// var to_string = function (x) { return String(x) }

//----------------------------------------------------------------------------

/* automatically dealt with js_of_ast.ml

val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( / ) : int -> int -> int
val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float
val ( = ) : float -> float -> bool
val ( < ) : float -> float -> bool
val ( > ) : float -> float -> bool
val ( <= ) : float -> float -> bool 
val ( >= ) : float -> float -> bool
*/

//----------------------------------------------------------------------------

var nat_eq = function(x, y) { return x === y; };
var int_eq = function(x, y) { return x === y; };
var int_lt = function(x, y) { return x < y; };
var int_gt = function(x, y) { return x > y; };
var int_le = function(x, y) { return x <= y; };
var int_ge = function(x, y) { return x >= y; };
var int_compare = function(x, y) { return x - y; };

//----------------------------------------------------------------------------

var int_of_float = function(x) { return x; };
var number_of_int = function(x) { return x; };
var of_int = function(x) { return x; };

//----------------------------------------------------------------------------

var number_comparable = function(x, y) {
  if (typeof(x) != "number" || typeof(y) != "number")
    throw "string_eq invalid arguments";
  return x === y; 
};


//----------------------------------------------------------------------------

var bool_eq = function(x, y) { return x === y; };

var not = function(x) { return !x; };


//----------------------------------------------------------------------------

var string_eq = function(x, y)  { 
  if (typeof(x) != "string" || typeof(y) != "string")
    throw "string_eq invalid arguments";
  return x === y; 
};

var string_compare = function(x, y) {
  return x.localeCompare(y);
};

var strappend = function(x, y) { 
  if (typeof(x) != "string" || typeof(y) != "string")
    throw "strappend invalid arguments";
  return x + y; 
};

var strlength = function(x) { 
  if (typeof(x) != "string")
    throw "strlength invalid arguments";
  return x.length;
};

var substring = function(n, m, s) { 
  if (typeof(s) != "string")
    throw "strlength invalid arguments";
  return s.slice(n, n+m); 
};


//----------------------------------------------------------------------------

/*
(* We use this to compare types that are not known by stdlib, like Native_error;
  should be implemented in JS by comparing the objects, to see if they have the same
  "tag" fields (there should be no other fields, except perhaps "type") *)
val ( === ) : 'a -> 'a -> bool  (* becomes === in js *)

*/

//----------------------------------------------------------------------------


var __LOC__ = "___LOC___"

var raise = function(x) { throw "Not_found"; };




