
// Source file loaded initially
var source_file = 'var x = alloc;\n{ x.foo = 12;\n  x.bar = x.foo;\n  x.cycle = x; }\n';

// Source code for the interpreter
// This code was generated from the code placed in comments in "interp.js",
// which was itself obtained by dropping the "log" and "ctx" lines from
// the instrumented interpreter just above.
var tracer_files = [
  { file: 'interp.js', contents: 'function run_trm(t) {\n  switch (t.tag) {\n  case "trm_var":\n    var v = lookup_var(t.name);\n    return res_val(v);\n  case "trm_cst":\n    return res_val({ tag: "val_cst", cst: t.cst });\n  case "trm_let":\n    return if_success(run_trm(t.t1), function(v1) {\n      env_push(t.name, v1);\n      var res = run_trm(t.t2);\n      env_pop();\n      return res;\n    });\n  case "trm_seq":\n    return if_success(run_trm(t.t1), function(v1) {\n      return if_success(run_trm(t.t2), function (v2) {\n        return(res_val(v2));        \n      });\n    });\n  case "trm_alloc":\n    var loc = heap_alloc();\n    return res_val({ tag: "val_loc", loc: loc });\n  case "trm_get":\n    return if_success(run_trm(t.loc), function(loc) {\n      var v = heap_get(loc, t.field);\n      return res_val(v);\n    });\n  case "trm_set":\n    return if_success(run_trm(t.loc), function(loc) {\n      return if_success(run_trm(t.arg), function(arg) {\n        heap_set(loc, t.field, arg);\n        return res_val(arg);\n      });\n    });\n  case "trm_if":\n    return if_success(run_trm(t.cond), function(cond) {\n      return if_bool(cond, function(b) {\n        if (b) {\n          return if_success(run_trm(t.then), function(v) {\n            return res_val(v);\n          });\n        } else if (t.else_option !== undefined) {\n          return if_success(run_trm(t.else), function(v) {\n            return res_val(v);\n          });\n        } else {\n          return res_val({tag:"val_cst", cst:{tag:"cst_bool", bool:true}});\n        }\n      });\n    });\n  }\n}' }
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