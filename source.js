
// Source file loaded initially
var source_file = '((1972 / 29) / 2) + 8';

// Source code for the interpreter
// This code was generated from the code placed in comments in "interp.js",
// which was itself obtained by dropping the "log" and "ctx" lines from
// the instrumented interpreter just above.
var tracer_files = [
  { file: 'interp.js', contents: 'function run_trm(expr) {\n  var ctx = ctx_empty();\n     return (function () {\n       switch (expr.type) {\n       case "Const": var n = expr.value;\n\n                     return n;\n       case "Add": var ls = expr.left, rs = expr.right;\n\n                   return run_trm(ls) + run_trm(rs);\n       case "Sub": var ls = expr.left, rs = expr.right;\n\n                   return run_trm(ls) - run_trm(rs);\n       case "Mul": var ls = expr.left, rs = expr.right;\n\n                   return run_trm(ls) * run_trm(rs);\n       case "Div": var ls = expr.left, rs = expr.right;\n\n                   return run_trm(ls) / run_trm(rs);\n       }\n     }());\n   };'
}
]

/* DEPRECATED
var tracer_items = [ 
{ type: 'enter', file: 'interp.ml', start_line: 4, start_col: 0, end_line: 5, end_col: 1 },
{ type: 'exit', file: 'interp.ml', start_line: 5, start_col: 1, end_line: 5, end_col: 1 },
{ type: 'enter', file: 'interp.ml', start_line: 5, start_col: 0, end_line: 5, end_col: 4 },
{ type: 'enter', file: 'interp.ml', start_line: 0, start_col: 4, end_line: 0, end_col: 1 },
{ type: 'exit', file: 'interp.ml', start_line: 0, start_col: 5, end_line: 0, end_col: 1 },
{ type: 'exit', file: 'interp.ml', start_line: 5, start_col: 1, end_line: 5, end_col: 4 },
];
*/
