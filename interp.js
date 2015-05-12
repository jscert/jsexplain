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

function log_custom(arg) {
  arg.heap = heap;
  arg.env = env;
  datalog.push(arg);
}

function log(line, ctx, type) {
  log_custom({line: line, ctx: ctx, type: type});
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


function lookup_var(x) {
  var e = env;
  while (e.tag === "env_cons") {
    if (e.name === x) {
      return e.val;
    }
    e = e.env;
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
  log_custom({type: "heap_set"});
}

function env_pop() {
  if (env.tag !== "env_cons")
    stuck("pop from empty env");
  env = env.env;
  log_custom({type: "env_pop"});
}

function env_push(x, v) {
  env = { tag: "env_cons", env: env, name: x, val: v, valkind: "value" };
  log_custom({type: "env_push"});
}

function ctx_empty() {
  return {tag: "env_nil"};
}

function ctx_push(ctx, name, value, typeoption) {
  return {tag: "env_cons", env: ctx, name: name, val: value, valkind: typeoption};
}

function run_trm_wrap(t) {
  log_custom({type: "enter"});
  var res = run_trm(t);
  log_custom({type: "exit"});
  return res;
}

function run_trm(t) {
  var run_trm = run_trm_wrap;
  var ctx = ctx_empty();
  ctx = ctx_push(ctx, "t", t, "term");
  log(1, ctx, "run_trm");
  switch (t.tag) {
  case "trm_var":
    log(2, ctx, "case");
    var v = lookup_var(t.name);
    ctx = ctx_push(ctx, "v", v, "value");
    log(2.5, ctx, "var");
    return res_val(v);
  case "trm_cst":
    log(3, ctx, "case");
    return res_val({ tag: "val_cst", cst: t.cst });
  case "trm_let":
    log(4, ctx, "case");
    return if_success(run_trm(t.t1), function(v1) {
      ctx = ctx_push(ctx, "v1", v1, "value");
      log(5, ctx, "fun");
      env_push(t.name, v1);
      var res = run_trm(t.t2);
      ctx = ctx_push(ctx, "res", res, "result");
      log(6, ctx, "var");
      env_pop();
      return res;
    });
  case "trm_seq":
    log(6.5, ctx, "case");
    return if_success(run_trm(t.t1), function(v1) {
      ctx = ctx_push(ctx, "v1", v1, "value");
      log(7, ctx, "fun");
      return if_success(run_trm(t.t2), function (v2) {
        ctx = ctx_push(ctx, "v2", v2, "value");
        log(8, ctx, "fun");
        return(res_val(v2));        
      });
    });
  case "trm_alloc":
    log(8.5, ctx, "case");
    var loc = heap_alloc();
    ctx = ctx_push(ctx, "loc", loc);
    log(9, ctx, "var");
    return res_val({ tag: "val_loc", loc: loc });
  case "trm_get":
    log(9.5, ctx, "case");
    return if_success(run_trm(t.loc), function(loc) {
      ctx = ctx_push(ctx, "loc", loc, "value");
      log(10, ctx, "fun");
      var v = heap_get(loc, t.field);
      ctx = ctx_push(ctx, "v", v, "value");
      log(11, ctx, "var");
      return res_val(v);
    });
  case "trm_set":
    log(11.5, ctx, "case");
    return if_success(run_trm(t.loc), function(loc) {
      ctx = ctx_push(ctx, "loc", loc, "value");
      log(12, ctx, "fun");
      return if_success(run_trm(t.arg), function(arg) {
        ctx = ctx_push(ctx, "arg", arg, "value");
        log(13, ctx, "fun");
        heap_set(loc, t.field, arg);
        return res_val(arg);
      });
    });
  case "trm_if":
    log(13.2, ctx, "case");
    return if_success(run_trm(t.cond), function(cond) {
      ctx = ctx_push(ctx, "cond", cond, "value");
      log(14, ctx, "fun");
      return if_bool(cond, function(b) {
        ctx = ctx_push(ctx, "b", b);
        log(15, ctx, "fun");
        if (b) {
          log(15.5, ctx, "case");
          return if_success(run_trm(t.then), function(v) {
            ctx = ctx_push(ctx, "v", v, "value");
            log(16, ctx, "fun");
            return res_val(v);
          });
        } else if (t.else_option !== undefined) {
          log(16.5, ctx, "case");
          return if_success(run_trm(t.else), function(v) {
            ctx = ctx_push(ctx, "v", v, "value");
            log(17, ctx, "fun");
            return res_val(v);
          });
        } else {
          log(18, ctx, "case");
          // res_unit
          return res_val({tag:"val_cst", cst:{tag:"cst_bool", bool:true}});
        }
      });
    });
  default:
    stuck("invalid trm tag");
  }
}

  function run_program(program) {
    for (var i = 0; i < program.length; i++) {
      run_trm_wrap(program[i]);
    }
  }


function trm_number(n) {
  return { tag: "trm_cst", cst: { tag: "cst_number", number: n } };
}

function trm_let(name, t1, t2) {
  return { tag: "trm_let", name: name, t1: t1, t2: t2 };
}

function trm_seq(t1, t2) {
  return { tag: "trm_seq", t1: t1, t2: t2 };
}

function trm_var(name) {
  return { tag: "trm_var", name: name };
}

var trm1 =
      trm_let("x", { tag: "trm_alloc"},
              trm_seq(trm_seq({tag: "trm_set", loc: trm_var("x"), field: "foo", arg: trm_number(12)},
                      {tag: "trm_set", loc: trm_var("x"), field: "bar",
                       arg: {tag:"trm_get", loc: trm_var("x"), field: "foo"}}),
                      {tag: "trm_set", loc: trm_var("x"), field: "cycle",
                       arg: trm_var("x")}));

var program = [trm1];

run_program(program);

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
    // case "val_constr":
    // case "val_record":
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
  var stack = [];
  while (env.tag === "env_cons") {
    stack.push(env);
    env = env.env;
  }
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

