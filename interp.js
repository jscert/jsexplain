


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


// types heap and env have special treatment: they are updated by side effects
var heap = [];
var env = { tag: "env_nil" };

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
  var loc = heap.length;
  heap[loc] = {};
  return loc;
}

// loc has type value (with a loc field), field is a string
function heap_get(loc, field) {
  var obj = heap[loc.loc];
  if (obj === undefined)
    stuck("unbound loc " + loc);
  var v = obj[field];
  if (v === undefined)
    stuck("unbound field " + field + " in loc " + loc);
  return v;
}

function heap_set(loc, field, arg) {
  var obj = heap[loc.loc];
  if (obj === undefined)
    stuck("unbound loc " + loc);
  obj[field] = arg;
}

function env_pop() {
  if (env.tag !== "env_cons")
    stuck("pop from empty env");
  env = env.env;
}

function env_push(x, v) {
  env = { tag: "env_cons", env: env, name: x, val: v }; 
}

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
      return run_trm(t.t2);
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
          return run_trm(t.then);
        } else if (t.else_option !== undefined) {
          return run_trm(t.else);
        } else {
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
      run_trm(program[i]);
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

  /*
   Definition run_trm R m t : result := 
   let run_trm := runs_trm R in
   match t with
   | trm_var x => res_stuck
   | trm_cst c => out_ter m c
   | trm_abs oy p t1 => 
   out_ter m (val_abs oy p t1)
   | trm_constr k ts => 
   runs_list R m ts nil (fun m vs => 
   out_ter m (val_constr k vs))
   | trm_record ats => res_unimplem
   | trm_unary f t1 => 
   if_success (run_trm m t1) (fun m v1 =>
   let ret v := out_ter m v in
   match f with
   | prim_neg => if_bool v1 (fun z => ret (neg z))
   | prim_not => if_int v1 (fun n => ret (-n))
   | _ => stuck "invalid unary operator"
   end)
   | trm_binary f t1 t2 => 
   if_success (run_trm m t1) (fun m v1 =>
   if_success (run_trm m t2) (fun m v2 =>
   let ret v := out_ter m v in
   let op_int F :=
   if_int v1 (fun n1 => if_int v2 (fun n2 => F n1 n2)) in
   match f with
   | prim_eq => run_primitive_eq m v1 v2
   | prim_add => op_int (fun n1 n2 => ret (n1+n2))
   | _ => stuck "invalid binary operator"
   end))
   | trm_app t1 t2 =>
   if_success (run_trm m t1) (fun m v1 =>
   if_success (run_trm m t2) (fun m v2 =>
   if_abs v1 (fun oy p t => 
   run_call R m oy p t v2)))
   | trm_seq t1 t2 =>
   if_success (run_trm m t1) (fun m v1 =>
   If v1 = val_unit 
   then (run_trm m t2)
   else stuck "sequence with LHS that is not unit")
   | trm_if t1 t2 t3o => 
   if_success_bool_cases (run_trm m t1)
   (fun m => run_trm m t2)
   (fun m =>
   match t3o with
   | None => out_ter m val_unit
   | Some t3 => run_trm m t3
   end)
   | trm_match t1 bs => 
   if_success (run_trm m t1) (fun m v1 =>
   let B := (beh_exn constr_matching_failure) in
   runs_branches R m B v1 bs)
   | trm_abort
   */


  function jsheap_of_heap(heap) {
    var jsheap = [];
    var i;
    
    for (i = 0; i < heap.length; i++) {
      jsheap[i] = {};
    }

    for (i = 0; i < heap.length; i++) {
      for (var x in heap[i]) {
        jsheap[i][x] = jsvalue_of_value(jsheap, heap[i][x]);
      }
    }

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


var j = jsheap_of_heap(heap)

