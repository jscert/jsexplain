var eval_ = function (expr) {

    return (function () {

    switch (expr.type) {
    
        case "Const": var n = expr.value;

                  return n;
  case "Add": var ls = expr.left, rs = expr.right;

                return add(eval_(ls), eval_(rs));
  case "Sub": var ls = expr.left, rs = expr.right;

                return sub(eval_(ls), eval_(rs));
  case "Mul": var ls = expr.left, rs = expr.right;

                return mul(eval_(ls), eval_(rs));
  case "Div": var ls = expr.left, rs = expr.right;

                return div(eval_(ls), eval_(rs));
  
        
    }
})();
    };


var print_expr = function (expr) {

    return (function () {

    switch (expr.type) {
    
        case "Const": var n = expr.value;

                  return to_string(n);
  case "Add": var ls = expr.left, rs = expr.right;

                return add(add(add(add("(", print_expr(ls)), ")"), " + "), print_expr(rs));
  case "Sub": var ls = expr.left, rs = expr.right;

                return add(add(add(add("(", print_expr(ls)), ")"), " - "), print_expr(rs));
  case "Mul": var ls = expr.left, rs = expr.right;

                return add(add(add(add("(", print_expr(ls)), ")"), " * "), print_expr(rs));
  case "Div": var ls = expr.left, rs = expr.right;

                return add(add(add(add("(", print_expr(ls)), ")"), " / "), print_expr(rs));
  
        
    }
})();
    };


var f = (function () {

    var source = parse("((1972 / 29) / 2) + 8");

    
    return print(add(add(print_expr(source), " = "), to_string(eval_(source))));
    
})();
