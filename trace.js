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