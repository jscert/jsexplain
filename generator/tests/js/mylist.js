var incr = function (i) {
                  return add(i, 1);
                  };




var head = function (d, l) {

    return (function () {

    switch (l.type) {
    
        case "Nil": 
                return d;
  case "Cons": var x = l.hd, xs = l.tl;

                 return x;
  
        
    }
})();
    };


var tail = function (d, l) {

    return (function () {

    switch (l.type) {
    
        case "Nil": 
                return d;
  case "Cons": var x = l.hd, xs = l.tl;

                 return xs;
  
        
    }
})();
    };


var init = function (l) {

    return (function () {

    switch (l.type) {
    
        case "Nil": 
                return {type: "Nil"};
  case "Cons": var x = l.hd, xs = l.tl;

                 return xs;
  
        
    }
})();
    };


var last = function (l) {

    return (function () {

    switch (l.type) {
    
        case "Nil": 
                return {type: "Nil"};
  case "Cons": var x = l.hd, xs = l.tl;

                 return (function () {

    switch (xs.type) {
    
        case "Nil": 
                return x;
  default: 
             return last(xs);
  
        
    }
})();
  
        
    }
})();
    };


var fold_left = function (f, acc, l) {

    return (function () {

    switch (l.type) {
    
        case "Nil": 
                return acc;
  case "Cons": var x = l.hd, xs = l.tl;

                 return fold_left(f, f(acc, x), xs);
  
        
    }
})();
    };


var fold_right = function (f, l, acc) {

    return (function () {

    switch (l.type) {
    
        case "Nil": 
                return acc;
  case "Cons": var x = l.hd, xs = l.tl;

                 return f(fold_right(f, xs, acc), x);
  
        
    }
})();
    };


var rev_map = function (f, l) {

    return fold_left(function (acc, x) {
                       return {type: "Cons", hd: f(x), tl: acc};
                       }, {type: "Nil"}, l);
    };


var map = function (f, l) {

    return fold_right(function (acc, x) {
                       return {type: "Cons", hd: f(x), tl: acc};
                       }, l, {type: "Nil"});
    };


var length = function (l) {

    return fold_left(function (acc, x) {
                       return incr(acc);
                       }, 0, l);
    };


var range = function (i, j, acc) {

    return (function () {

    if (le(i, j)) {
    
        return  range(incr(i), j, {type: "Cons", hd: i, tl: acc});
    } else {
    
        return  acc;
    }
})();
    };


var list0 = {type: "Nil"};


var list1 = range(0, 1, {type: "Nil"});


var list2 = range(1, 5, {type: "Nil"});


var sqr = function (x) {
                  return mul(x, x);
                  };


length(list0)

length(list1)

length(list2)

map(sqr, list0)

map(sqr, list1)

map(sqr, list2)