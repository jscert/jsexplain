

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

