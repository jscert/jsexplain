/*
  User provided info: // cannot use "tag" and "type"

    cst_bool => "bool"
    cst_number => "number"

    val_loc => "loc"
    ...

    trm_var => "name"
    trm_cst => "cst"
    trm_let => "name", "t1", "t2"
    trm_get => "loc" "field"
    trm_set => "loc", "field", "arg"
    trm_if  => "cond", "then", "else_option"


  Encoding of Caml algebraic data types into objects
   cst_number 5   =>  { type: "cst", tag: "cst_number", val: 5 }
   val_abs(e,r,p,t)   =>  { type: "val", tag: "val_abs", 
                            env:e, recname:r, pattern:p, body:t }
   special case for option: undefined
*/

/* 
  type var_name = string
  type field_name = string
  type tag_id = int
  type loc = int

  type class_id = int
  type branch_info = { tag_name : string; args_names : array<string> }
  type constr_info = array<array<branch_info>> 
  type record_info = array<string>  // name of the type of the record

  type cst =
     | cst_bool of bool
     | cst_number of number

  type env = 
    | env_nil 
    | env_cons of env * var_name * val
    
  type val = 
     | val_cst of cst
     | val_loc of loc
     | val_abs of env * option var_name * pat * trm 
     | val_constr of class_id * tag_id * array<val>
     | val_record of class_id * { field_name -> val }

  type heap = array<val> // indexed by loc

  type prim = 
     | prim_eq
     | prim_neg
     | prim_add
     | prim_and
     | prim_or
     | prim_not

  type pat =
     | pat_var of var_name
     | pat_wild
     | pat_alias of pat * var_name
     | pat_or of pat * pat
     | pat_cst of cst
     | pat_constr of tag_name * { tag_name -> val }
     | pat_record of { field_name -> pat }
  
  type trm =
     | trm_var of var_name
     | trm_cst of cst
     | trm_abs of option var_name * pat * trm 
     | trm_constr of class_id * tag_id * array<trm>
     | trm_record of class_id * { field_name -> trm }
     | trm_unary of prim * trm 
     | trm_binary of prim * trm * trm 
     | trm_app of trm * trm 
     | trm_seq of trm * trm
     | trm_let of pat * trm * trm
     | trm_alloc
     | trm_get of trm * field_name
     | trm_set of trm * field_name * trm
     | trm_if of trm * trm * option trm 
     | trm_match of trm * array<branch>
     | trm_abort
     (* | trm_while of trm * trm
        | trm_for of var_name * trm * trm * trm  *)

   type program = array<trm>

   type branch =
     | branch_intro of pat * trm

   type res = 
     | res_val of val
     | res_abort

*/

// types heap and env have special treatment: they are updated by side effects
var heap = new PersistentArray([]);
var env = { tag: "env_nil" };

var datalog = [];

function log_reset() {
  datalog = [];
}

function log_custom(arg) {
  if (arg.file === undefined) {
    console.log("Undefined file name in log");
    arg.file = "calc.js";    
  }
  arg.heap = heap;
  arg.env = env;
  arg.start_line = arg.line;
  arg.start_col = 0;
  arg.end_line = arg.line;
  arg.end_col = 1000;
  datalog.push(arg);
}

function log(line, ctx, type) {
  log_custom({line: line, ctx: ctx, type: type });
}

function stuck(msg) {
  throw { type: "stuck", msg: msg };
}

// all monads have return type "res"

function if_bool(v, K) {
  if (v.tag === "cst_bool") {
    return K(v.bool);
  } else {
    stuck("boolean value was expected");
  }
}
  
function if_number(v, K) {
  if (v.tag === "cst_number") {
    return K(v.number);
  } else {
    stuck("int value was expected");
  }
}

function if_abs(v, K) {
  if (v.tag === "cst_abs") {
    return K(v.env, v.recname, v.pattern, v.body);
  } else {
    stuck("abs value was expected");
  }
}

function if_success(res, K) {
  if (res.tag === "res_val") {
    return K(res.val);
  } else {
    return res;
  }
}

function if_success_bool_cases(res, K1, K2) {
  return if_success(res, function(v) {
    return if_bool(v, function(b) {
      if (b) {
        return K1();
      } else {
        return K2();
      }
    });
  });
}

function array_of_env(env) {
  var a = [];

  while (env.tag === "env_cons") {
    a.push(env);
    env = env.env;
  }

  return a;
}

function lookup_var(x) {
  var ae = array_of_env(env);
  for (var i = 0; i < ae.length; i++){
    if (ae[i].name === x) {
      return ae[i].val;
    }
  }
  stuck("unbound variable " + x);
}

function res_val(v) {
  return { tag: "res_val", val: v };
}


function heap_alloc() {
  var loc = heap.length();
  var obj = new PersistentArray({});
  heap = heap.copyAndSet(loc, obj);
  return loc;
}

// loc has type value (with a loc field), field is a string
function heap_get(loc, field) {
  var obj = heap.get(loc.loc);
  if (obj === undefined)
    stuck("unbound loc " + loc);
  var v = obj.get(field);
  if (v === undefined)
    stuck("unbound field " + field + " in loc " + loc);
  return v;
}

function heap_set(loc, field, arg) {
  var obj = heap.get(loc.loc); // obj:PersistentArray
  if (obj === undefined)
    stuck("unbound loc " + loc);
  var objnew = obj.copyAndSet(field, arg);
  heap = heap.copyAndSet(loc.loc, objnew);
}

function env_pop() {
  if (env.tag !== "env_cons")
    stuck("pop from empty env");
  env = env.env;
}

function env_push(x, v) {
  env = { tag: "env_cons", env: env, name: x, val: v, valkind: "value" };
}

function ctx_empty() {
  return {tag: "env_nil"};
}

function ctx_push(ctx, name, value, typeoption) {
  return {tag: "env_cons", env: ctx, name: name, val: value, valkind: typeoption};
}

function run_trm_wrap(line, t) {
  log_custom({line: line, type: "enter"});
  var res = run_trm(t);
  log_custom({line: line, type: "exit"});
  return res;
}

function run_trm(expr) {
  var ctx = ctx_empty();
  ctx = ctx_push(ctx, "expr", expr, "term");
  log(1, ctx, "run_trm");
  return (function (expr) {
          var Stack = {
            is_empty: function (s) {
              return s === {type: "N"};
            },

            push: function (x, stack) {
              return {type: "C", value: x, stack: stack};
            },

            pop: function (stack) {
              return (function () {
                switch (stack.type) {
                  case "C": var x = stack.value, xs = stack.stack;
                  return x;
                  case "K": var x = stack.value, xs = stack.stack;
                  return x;
                  case "B": 
                  return stuck("Empty list");
                  case "N": 
                  return stuck("Empty list");
                }
              }())
              ;
            },

          }

          var eval_ = function (expr) {
           return (function () {
             switch (expr.type) {
               case "Const": var n = expr.value;
               ctx_push(ctx, "n", n, "value");
               log(30 , ctx, "Const");

               return n;
               case "Add": var ls = expr.left, rs = expr.right;
               ctx_push(ctx, "ls", ls, "value");
               ctx_push(ctx, "rs", rs, "value");
               log(32 , ctx, "Add");

               return eval_(ls) + eval_(rs);
               case "Sub": var ls = expr.left, rs = expr.right;
               ctx_push(ctx, "ls", ls, "value");
               ctx_push(ctx, "rs", rs, "value");
               log(34 , ctx, "Sub");

               return eval_(ls) - eval_(rs);
               case "Mul": var ls = expr.left, rs = expr.right;
               ctx_push(ctx, "ls", ls, "value");
               ctx_push(ctx, "rs", rs, "value");
               log(36 , ctx, "Mul");

               return eval_(ls) * eval_(rs);
               case "Div": var ls = expr.left, rs = expr.right;
               ctx_push(ctx, "ls", ls, "value");
               ctx_push(ctx, "rs", rs, "value");
               log(38 , ctx, "Div");

               return eval_(ls) / eval_(rs);
               case "Pop": var s = expr.stack;
               ctx_push(ctx, "s", s, "value");
               log(40 , ctx, "Pop");

               return Stack.pop(evals(s));
             }
           }())
      ;
      };

      var evals = function (sexpr) {
        return (function () {
          switch (sexpr.type) {
            case "Emp": 
            return {type: "Stack.N"};
            case "Push": var v = sexpr.value, s = sexpr.stack;
            ctx_push(ctx, "v", v, "value");
            ctx_push(ctx, "s", s, "value");
            log(52 , ctx, "Push");

            return Stack.push(eval_(v), evals(s));
          }
        }())
        ;
      };

      var print_expr = function (expr) {
        return (function () {
          switch (expr.type) {
            case "Const": var n = expr.value;
            ctx_push(ctx, "n", n, "value");
            log(62 , ctx, "Const");

            return to_string(n);
            case "Add": var ls = expr.left, rs = expr.right;
            ctx_push(ctx, "ls", ls, "value");
            ctx_push(ctx, "rs", rs, "value");
            log(64 , ctx, "Add");

            return "(" + print_expr(ls) + ")" + " + " + print_expr(rs);
            case "Sub": var ls = expr.left, rs = expr.right;
            ctx_push(ctx, "ls", ls, "value");
            ctx_push(ctx, "rs", rs, "value");
            log(66 , ctx, "Sub");

            return "(" + print_expr(ls) + ")" + " - " + print_expr(rs);
            case "Mul": var ls = expr.left, rs = expr.right;
            ctx_push(ctx, "ls", ls, "value");
            ctx_push(ctx, "rs", rs, "value");
            log(68 , ctx, "Mul");

            return "(" + print_expr(ls) + ")" + " * " + print_expr(rs);
            case "Div": var ls = expr.left, rs = expr.right;
            ctx_push(ctx, "ls", ls, "value");
            ctx_push(ctx, "rs", rs, "value");
            log(70 , ctx, "Div");

            return "(" + print_expr(ls) + ")" + " / " + print_expr(rs);
            case "Pop": var s = expr.stack;
            ctx_push(ctx, "s", s, "value");
            log(72 , ctx, "Pop");

            return "Pop(" + print_sexpr(s) + ")";
          }
        }())
      ;
      };

      var print_sexpr = function (sexpr) {
        return (function () {
          switch (sexpr.type) {
            case "Emp": 
            return "Emp";
            case "Push": var v = sexpr.value, s = sexpr.stack;
            ctx_push(ctx, "v", v, "value");
            ctx_push(ctx, "s", s, "value");
            log(84 , ctx, "Push");

            return "Push(" + print_expr(v) + ", " + print_sexpr(s) + ")";
          }
        }())
        ;
      };

        return eval_(expr);
     }(expr))
     ;
   };


/* Same as above
   Used in "trace.js"   

function run_trm(t) {
  switch (t.tag) {
  case "trm_var":
    var v = lookup_var(t.name);
    return res_val(v);
  case "trm_cst":
    return res_val({ tag: "val_cst", cst: t.cst });
  case "trm_let":
    return if_success(run_trm(t.t1), function(v1) {
      env_push(t.name, v1);
      var res = run_trm(t.t2);
      env_pop();
      return res;
    });
  case "trm_seq":
    return if_success(run_trm(t.t1), function(v1) {
      return if_success(run_trm(t.t2), function (v2) {
        return(res_val(v2));        
      });
    });
  case "trm_alloc":
    var loc = heap_alloc();
    return res_val({ tag: "val_loc", loc: loc });
  case "trm_get":
    return if_success(run_trm(t.loc), function(loc) {
      var v = heap_get(loc, t.field);
      return res_val(v);
    });
  case "trm_set":
    return if_success(run_trm(t.loc), function(loc) {
      return if_success(run_trm(t.arg), function(arg) {
        heap_set(loc, t.field, arg);
        return res_val(arg);
      });
    });
  case "trm_if":
    return if_success(run_trm(t.cond), function(cond) {
      return if_bool(cond, function(b) {
        if (b) {
          return if_success(run_trm(t.then), function(v) {
            return res_val(v);
          });
        } else if (t.else_option !== undefined) {
          return if_success(run_trm(t.else), function(v) {
            return res_val(v);
          });
        } else {
          return res_val({tag:"val_cst", cst:{tag:"cst_bool", bool:true}});
        }
      });
    });
  }
}

*/


function run_program(program) {
  log_reset();
  run_trm_wrap(0, program);
}

//----------------demo---------------


var parse = function (source) {
    var ast = esprima.parse(source).body[0].expression;

    function transform (tree) {
      if (tree === undefined) {
      } else {
            // Javascript style instructions are well handled with no additional creation.
            switch (tree.operator) {
              case '+':
              tree.type = "Add"; tree.operator = undefined; break;
              case '-':
              tree.type = "Sub"; tree.operator = undefined; break;
              case "*":
              tree.type = "Mul"; tree.operator = undefined; break;
              case "/":
              tree.type = "Div"; tree.operator = undefined; break;
              default: break;
            }

            if (tree.type === "Literal") {
              tree.type = "Const";
            }

            // tree.left and tree.right from parser interpretation
            if (tree.left !== undefined) tree.left = transform(tree.left);
            if (tree.right !== undefined) tree.right = transform(tree.right);

            // Esprima sees these standalone type instructions as Identifiers
            if (tree.type === "Identifier") {
              switch (tree.name) {
                case "Emp":
                tree.type = "Emp"; break;
                default: console.log("Unknown ident"); break;
              }
            }

            // Esprima generates a function type structure that needs reshaping to the
            // automatically generated structure, respecting label names!
            if (tree.type === "CallExpression") {
              switch (tree.callee.name) {
                case "Push":
                tree.type = "Push"; 
                tree.value = transform(tree.arguments[0]);
                tree.stack = transform(tree.arguments[1]);
                delete tree.arguments;
                break;
                case "Pop":
                tree.type = "Pop"; 
                tree.stack = transform(tree.arguments[0]);
                delete tree.arguments;
                break;
                default: console.log("Unknown callexpr"); break;
              }   
            }

            return tree;
          }
        }

    return transform(ast);
}

var program;


//----------------reporting---------------

function jsheap_of_heap(heap) {
  var jsheap = [];
  var i;
    
  for (i = 0; i < heap.length(); i++) {
    jsheap[i] = {_loc: i}; // The field “_loc” is optional.
  }

  for (i = 0; i < heap.length(); i++) {
    // obj is an object
    var obj = heap.get(i);
    if (obj === undefined) break;
    obj = obj.asReadOnlyArray();
    for (var x in obj) {
      if (obj[x] === undefined) continue;
      jsheap[i][x] = jsvalue_of_value(jsheap, obj[x]);
    }
  }
  jsheap.length = i; // TODO Arthur

  return jsheap;
}

function jsvalue_of_cst(c) {
  switch (c.tag) {
  case "cst_bool":
    return c.bool;
  case "cst_number":
    return c.number;
  default:
    stuck("unrecognized cst");
  }
}

function jsvalue_of_value(jsheap, v) {
  switch (v.tag) {
  case "val_cst":
    return jsvalue_of_cst(v.cst);
  case "val_loc":
    return jsheap[v.loc];
  case "val_abs":
    return "<closure>";
  default:
    stuck("unrecognized value");
  }
}

function jsresult_of_result(jsheap, res) {
  if (res.tag === "res_abort") {
    return undefined;
  } else if (res.tag === "res_val") {
    return jsvalue_of_value(jsheap, res.val);
  } else stuck("Unknown result");
}

function jsenv_of_env(jsheap, env) {
  var obj = {};
  var stack = array_of_env(env);
  while (stack.length > 0) {
    var iv = stack.pop();
    switch (iv.valkind) {
    case "term":
      obj[iv.name] = iv.val;
      break;
    case "value":
      obj[iv.name] = jsvalue_of_value(jsheap, iv.val);
      break;
    case "result":
      obj[iv.name] = jsresult_of_result(jsheap, iv.val);
      break;
    default:
      obj[iv.name] = iv.val;
      break;
    }
  }
  return obj;
}

//----------------AST conversion---------------

function isNumeric(num) {
  return !isNaN(num);
}
  

function esprimaExprToAST(expr) {
  var res;
  switch (expr.type) {
  case "Literal":
    var value = expr.value;
    if (!isNumeric(value)) throw ("Literal not a number: " + value);
    res = trm_number(expr.loc.start.line, expr.value);
    res.start = expr.loc.start;
    res.end = expr.loc.end;
    return res;
  case "Identifier":
    switch (expr.name) {
    case "alloc":
      res = trm_alloc(expr.loc.start.line);
      res.start = expr.loc.start;
      res.end = expr.loc.end;
      return res;
    default:
      res = trm_var(expr.loc.start.line, expr.name);
      res.start = expr.loc.start;
      res.end = expr.loc.end;
      return res;
    }
  case "AssignmentExpression":
    if (expr.operator !== "=") throw ("AssignmentExpression NI: " + expr.operator);
    if (expr.left.type !== "MemberExpression") throw ("Expected MemberExpression");
    if (expr.left.property.type !== "Identifier") throw ("Expected Identifier");
    res = trm_set(expr.loc.start.line,
                  esprimaExprToAST(expr.left.object),
                  expr.left.property.name,
                  esprimaExprToAST(expr.right));
    res.start = expr.loc.start;
    res.end = expr.loc.end;    
    return res;
  case "MemberExpression":
    if (expr.property.type !== "Identifier") throw ("Expected Identifier");
    res =  trm_get(expr.loc.start.line,
                   esprimaExprToAST(expr.object),
                   expr.property.name);
    res.start = expr.loc.start;
    res.end = expr.loc.end;    
    return res;
  default: return "Expr NI";
  }
}

function esprimaSeqToAST(stats) {
  var state = {prog: stats, index: 0};
  var seql = [];
  var prev;
  while (state.index < state.prog.length) {
    seql.push(esprimaStatsToAST(state));
  }
  if (seql.length === 0) throw "Empty block";
  var res = seql.pop();
  var end = res.end;
  while (seql.length > 0) {
    prev = seql.pop();
    res = trm_seq(prev.line, prev, res);
    res.start = prev.start;
    res.end = end;
  }
  return res;
}

function esprimaStatsToAST(state) {
  var stat = state.prog[state.index];
  state.index++;
  var res;
  switch (stat.type) {
  case "VariableDeclaration":
    var decl = stat.declarations[0];
    var next = state.prog[state.index];
    state.index++;
    if (next.type !== "BlockStatement") throw ("Expected Block, got: " + next.type);
    res = trm_let(decl.loc.start.line,
                  decl.id.name,
                  esprimaExprToAST(decl.init),
                  esprimaSeqToAST(next.body));
    res.start = stat.loc.start;
    res.end = next.loc.end;
    return res;
  case "ExpressionStatement":
    res = esprimaExprToAST(stat.expression);
    res.start = stat.loc.start;
    res.end = stat.loc.end;
    return res;
  default: return "Stat NI";
  }
}

function esprimaToAST(prog) {
  var conv = [];
  var state = {prog: prog.body, index: 0};
  while (state.index < state.prog.length) {
    conv.push(esprimaStatsToAST(state));
  }
  return conv;
}

/* demo
var j = jsheap_of_heap(heap);

for (var k = 0; k < datalog.length; k++) {
  var item = datalog[k];
  var jsheap = jsheap_of_heap(item.heap);
  item.heap = jsheap;
  item.env = jsenv_of_env(jsheap, item.env);
  if (item.ctx !== undefined) {
    item.ctx = jsenv_of_env(jsheap, item.ctx);
  }
}

*/
