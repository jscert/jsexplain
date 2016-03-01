
var mk_nil = function() {
   return { type: "list", tag: "[]" };
};

var mk_cons = function(head, tail) {
   return { type: "list", tag: "::", head: head, tail: tail };
};

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

var stuck = function (msg) { throw {type:'stuck', msg:msg} }

var to_string = function (x) { return String(x) }

var parse = function (source) {
    var ast = require('esprima').parse(source).body[0].expression;

    // Esprima does it's little thing. To be handled by auto generated code we need to 
    // reshape the tree structure to match the labels and expected content.

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
                        break;
                    case "Pop":
                        tree.type = "Pop"; 
                        tree.stack = transform(tree.arguments[0]);
                        break;
                    default: console.log("Unknown callexpr"); break;
                }   
            }

            return tree;
        }
    }

    return transform(ast);
}
