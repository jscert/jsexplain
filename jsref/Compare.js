/* Calls to these comparison functions are inserted into the
 * generated code in place of ===
 */

var _compare_basic_types = function(x, y) {
   return (x.tag === y.tag);
};

var _compare_JsSyntax_mathop = _compare_basic_types;
var _compare_JsSyntax_native_error = _compare_basic_types;
var _compare_JsSyntax_binary_op = _compare_basic_types;
var _compare_JsSyntax_restype = _compare_basic_types;
var _compare_JsSyntax_codetype = _compare_basic_types;
var _compare_JsSyntax_coq_type = _compare_basic_types;
var _compare_JsSyntax_mutability = _compare_basic_types;
var _compare_JsSyntax_mathop = _compare_basic_types;

var _compare_JsSyntax_undef_descriptor = function(x, y) {
  return (x.tag === y.tag) &&
    (x.tag != "Descriptor" || _compare_JsSyntax_descriptor(x.descriptor, y.descriptor));
};

var _compare_JsSyntax_value = function (v1, v2) {
  switch (v1.tag) {
    case "Coq_value_prim":
      var w1 = v1.value;
      switch (v2.tag) {
        case "Coq_value_prim":
          var w2 = v2.value;
          return (prim_compare(w1, w2));
        case "Coq_value_object":
          var o = v2.value;
          return (false);
      }

    case "Coq_value_object":
      var l1 = v1.value;
      switch (v2.tag) {
        case "Coq_value_prim":
          var p = v2.value;
          return (false);
        case "Coq_value_object":
          var l2 = v2.value;
          return (object_loc_compare(l1, l2));
      }
  }
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

