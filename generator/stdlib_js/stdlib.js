var add = function (a, b) { return a + b }
var sub = function (a, b) { return a - b }
var mul = function (a, b) { return a * b }
var div = function (a, b) { return a / b }

var eq = function (a, b) { return a === b }
var le = function (a, b) { return a < b }
var ge = function (a, b) { return a > b }

var leq = function (a, b) { return a <= b }
var geq = function (a, b) { return a >= b }

var print = function (x) { console.log(x) }

var to_string = function (x) { return String(x) }

var parse = function (source) {
    var ast = require('esprima').parse(source).body[0].expression;

    function transform (tree) {
        if (tree === undefined) {
        } else {
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

            switch (tree.type) {
                case "Literal":
                    tree.type = "Const"; break;
                default: break;
            }

            if (tree.left !== undefined) tree.left = transform(tree.left);
            if (tree.right !== undefined) tree.right = transform(tree.right);
            return tree;
        }
    }

    return transform(ast);
}
