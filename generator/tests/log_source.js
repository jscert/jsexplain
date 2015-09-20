function run_trm (code) {

var ctx = ctx_empty();

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
    ctx = ctx_push(ctx, "t", expr, "term");
    log(1, ctx, "run_trm");
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

return eval_(code);
};