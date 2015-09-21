function run_trm (code) {

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
    case "N": 
              return stuck("Empty list");
    }
  }())
  ;
},

}

var eval_ = function (expr) {
    var ctx = ctx_empty();
    ctx = ctx_push(ctx, "t", expr, "term");
    log(23, ctx, "eval_");
     return (function () {
       switch (expr.type) {
       case "Const": var n = expr.value;
                     ctx_push(ctx, "n", n, "value");
                     log(26 , ctx, "Const");
                     return n;

       case "Add": var ls = expr.left, rs = expr.right;
                   ctx_push(ctx, "ls", ls, "value");
                   ctx_push(ctx, "rs", rs, "value");
                   log(28 , ctx, "Add");
                   return call_wrap(29, ls, eval_) + call_wrap(29, rs, eval_);

       case "Sub": var ls = expr.left, rs = expr.right;
                   ctx_push(ctx, "ls", ls, "value");
                   ctx_push(ctx, "rs", rs, "value");
                   log(30 , ctx, "Sub");
                   return call_wrap(31, ls, eval_) - call_wrap(31, rs, eval_);

       case "Mul": var ls = expr.left, rs = expr.right;
                   ctx_push(ctx, "ls", ls, "value");
                   ctx_push(ctx, "rs", rs, "value");
                   log(32 , ctx, "Mul");
                   return call_wrap(33, line, eval_) * call_wrap(33, rs, eval_);

       case "Div": var ls = expr.left, rs = expr.right;
                   ctx_push(ctx, "ls", ls, "value");
                   ctx_push(ctx, "rs", rs, "value");
                   log(34 , ctx, "Div");
                   return call_wrap(35, ls, eval_) / call_wrap(35, rs, eval_);

       case "Pop": var s = expr.stack;
                   ctx_push(ctx, "s", s, "value");
                   log(36 , ctx, "Pop");
                   return Stack.pop(call_wrap(37, s, evals));
       }
     }())
     ;
   };
   
var evals = function (sexpr) {
  var ctx = ctx_empty();
  
  return (function () {
    switch (sexpr.type) {
    case "Emp": 
                return {type: "Stack.N"};
    case "Push": var v = sexpr.value, s = sexpr.stack;
                 ctx_push(ctx, "v", v, "value");
                 ctx_push(ctx, "s", s, "value");
                 log(48 , ctx, "Push");
                 return Stack.push(call_wrap(49, v, eval_), call_wrap(49, s, evals));

    }
  }())
  ;
};

return eval_(code);
};