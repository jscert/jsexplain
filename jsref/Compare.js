/* Calls to these comparison functions are inserted into the
 * generated code in place of ===
 *
 * Sorted alphabetically, to help with `grep -ho "_compare_[^(]*" jsref/`
 */

var _compare_basic_types = function(x, y) {
   return (x.tag === y.tag);
};

var _compare_JsCommon_ref_kind = _compare_basic_types;
var _compare_JsSyntax_binary_op = _compare_basic_types;
var _compare_JsSyntax_codetype = _compare_basic_types;
var _compare_JsSyntax_coq_type = _compare_basic_types;
var _compare_JsSyntax_mathop = _compare_basic_types;
var _compare_JsSyntax_mutability = _compare_basic_types;

var _compare_JsSyntax_object_loc = function (l1, l2) {
  switch (l1.tag) {
    case "Coq_object_loc_normal":
      var ln1 = l1.address;
      switch (l2.tag) {
        case "Coq_object_loc_normal":
          var ln2 = l2.address;
          return (nat_eq(ln1, ln2));
        case "Coq_object_loc_prealloc":
          var p = l2.prealloc;
          return (false);
      }

    case "Coq_object_loc_prealloc":
      var bl1 = l1.prealloc;
      switch (l2.tag) {
        case "Coq_object_loc_normal":
          var n = l2.address;
          return (false);
        case "Coq_object_loc_prealloc":
          var bl2 = l2.prealloc;
          return (_compare_JsSyntax_prealloc(bl1, bl2));
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

var _compare_JsSyntax_restype = _compare_basic_types;
var _compare_JsSyntax_resvalue_type = _compare_basic_types;

var _compare_JsSyntax_undef_descriptor = function(x, y) {
  return (x.tag === y.tag) &&
    (x.tag != "Descriptor" || _compare_JsSyntax_descriptor(x.descriptor, y.descriptor));
};

var _compare_JsSyntax_value = function (v1, v2) {
  switch (v1.tag) {
    case "Coq_value_undef":
      switch (v2.tag) {
        case "Coq_value_undef":
          return (true);
        default:
          return (false);
      }

    case "Coq_value_null":
      switch (v2.tag) {
        case "Coq_value_null":
          return (true);
        default:
          return (false);
      }

    case "Coq_value_bool":
      var b1 = v1.value;
      switch (v2.tag) {
        case "Coq_value_bool":
          var b2 = v2.value;
          return (bool_eq(b1, b2));
        default:
          return (false);
      }

    case "Coq_value_number":
      var n1 = v1.value;
      switch (v2.tag) {
        case "Coq_value_number":
          var n2 = v2.value;
          return ((n1 === n2));
        default:
          return (false);
      }

    case "Coq_value_string":
      var s1 = v1.value;
      switch (v2.tag) {
        case "Coq_value_string":
          var s2 = v2.value;
          return (string_eq(s1, s2));
        default:
          return (false);
      }

    case "Coq_value_object":
      var l1 = v1.value;
      switch (v2.tag) {
        case "Coq_value_object":
          var l2 = v2.value;
          return (_compare_JsSyntax_object_loc(l1, l2));
        default:
          return (false);
      }
  }
};


