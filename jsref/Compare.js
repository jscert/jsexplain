/* Calls to these comparison functions are inserted into the
 * generated code in place of ===
 */

/* Compare floating point numbers.
 * +0 = +0, -0 = -0, but +0 <> -0, and -0 <> +0
 * NaN = NaN
 */
var _compare_JsNumber_number = function(x, y) {
  if (x === 0 && y === 0) {
    return (1/x > 0 && 1/y > 0) || (1/x < 0 && 1/y < 0);
  }
  return (Math.isNaN(x) && Math.isNaN(y)) || (x === y);
};

var _compare_JsSyntax_mathop = function(x, y) {
   return (x.tag == y.tag);
};

var _compare_JsSyntax_native_error = function(x, y) {
   return (x.tag == y.tag);
};

var _compare_JsSyntax_prealloc = function(x, y) {
   return (x.tag == y.tag) 
     && (x.tag != "Coq_prealloc_mathop" 
         || _compare_JsSyntax_mathop(x.mathop, y.mathop))
     && (x.tag != "Coq_prealloc_native_error" 
         || _compare_JsSyntax_native_error(x.error, y.error))
     && (x.tag != "Coq_prealloc_native_error_proto" 
         || _compare_JsSyntax_native_error(x.error, y.error))
     ;
};

