
var is_empty = function (s) {
   return s === {type: "N"};
 };
 
var push = function (x, stack) {
  return {type: "C", value: x, stack: stack};
};

var pop = function (stack) {
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
};
