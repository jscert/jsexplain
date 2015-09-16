
// Source file loaded initially
var source_file = 'Pop(Push(1, Push(5, Emp))) - 7';

// Source code for the interpreter
// This code was generated from the code placed in comments in "interp.js",
// which was itself obtained by dropping the "log" and "ctx" lines from
// the instrumented interpreter just above.
var tracer_files = [
  { file: 'interp.js', contents: 'var Stack = {\n  is_empty: function (s) {\n    return s === {type: "N"};\n  },\n\n  push: function (x, stack) {\n    return {type: "C", value: x, stack: stack};\n  },\n\n  pop: function (stack) {\n    return (function () {\n      switch (stack.type) {\n        case "C": var x = stack.value, xs = stack.stack;\n        return x;\n        case "K": var x = stack.value, xs = stack.stack;\n        return x;\n        case "B": \n        return stuck("Empty list");\n        case "N": \n        return stuck("Empty list");\n      }\n    }())\n    ;\n  },\n\n}\nvar eval_ = function (expr) {\n     return (function () {\n       switch (expr.type) {\n       case "Const": var n = expr.value;\n                     return n;\n       case "Add": var ls = expr.left, rs = expr.right;\n                   return eval_(ls) + eval_(rs);\n       case "Sub": var ls = expr.left, rs = expr.right;\n                   return eval_(ls) - eval_(rs);\n       case "Mul": var ls = expr.left, rs = expr.right;\n                   return eval_(ls) * eval_(rs);\n       case "Div": var ls = expr.left, rs = expr.right;\n                   return eval_(ls) / eval_(rs);\n       case "Pop": var s = expr.stack;\n                   return Stack.pop(evals(s));\n       }\n     }())\n     ;\n   };\n   \nvar evals = function (sexpr) {\n  return (function () {\n    switch (sexpr.type) {\n    case "Emp": \n                return {type: "Stack.N"};\n    case "Push": var v = sexpr.value, s = sexpr.stack;\n                 return Stack.push(eval_(v), evals(s));\n    }\n  }())\n  ;\n};\n\nvar print_expr = function (expr) {\n  return (function () {\n    switch (expr.type) {\n    case "Const": var n = expr.value;\n                  return to_string(n);\n    case "Add": var ls = expr.left, rs = expr.right;\n                return "(" + print_expr(ls) + ")" + " + " + print_expr(rs);\n    case "Sub": var ls = expr.left, rs = expr.right;\n                return "(" + print_expr(ls) + ")" + " - " + print_expr(rs);\n    case "Mul": var ls = expr.left, rs = expr.right;\n                return "(" + print_expr(ls) + ")" + " * " + print_expr(rs);\n    case "Div": var ls = expr.left, rs = expr.right;\n                return "(" + print_expr(ls) + ")" + " / " + print_expr(rs);\n    case "Pop": var s = expr.stack;\n                return "Pop(" + print_sexpr(s) + ")";\n    }\n  }())\n  ;\n};\n\nvar print_sexpr = function (sexpr) {\n  return (function () {\n    switch (sexpr.type) {\n    case "Emp": \n                return "Emp";\n    case "Push": var v = sexpr.value, s = sexpr.stack;\n                 return "Push(" + print_expr(v) + ", " + print_sexpr(s) + ")";\n    }\n  }())\n  ;\n};\n\nreturn eval_(expr);'
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
