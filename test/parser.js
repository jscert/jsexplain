"use strict";

const assert = require('assert').strict;
const util = require('util');

var esprima = require('esprima');
var esprimaToAST = require('../esprima-to-ast.js');

var test262 = require('./helpers/test262.js');

/* Tests whether a given test is negative.
 * Param: negative (boolean or string): Return value of helper-test262.testNegativity
 * Returns: a string if type of failure specified, true, or false
 */
function isParserNegativeTest(negative) {
  if (typeof negative === 'undefined') {
    return false;
  }
  return (negative.phase === 'parse' || negative.phase === 'early') ? negative.type : false;
}

function typecheckAST(ast) {
  /* typename -> ((tagname -> fieldname -> type)|type|type => (tagname -> fieldname -> type)) */
  const types = {
    list: function (x) {return {"[]": {}, "::": {head: x, tail: x + " list"}}},
    option: function (x) {return {None: {}, Some: {value: x}}},
    unary_op: {
      Unary_op_delete: {}, Unary_op_void: {}, Unary_op_typeof: {}, Unary_op_post_incr: {},
      Unary_op_post_decr: {}, Unary_op_pre_incr: {}, Unary_op_pre_decr: {}, Unary_op_add: {},
      Unary_op_neg: {}, Unary_op_bitwise_not: {}, Unary_op_not: {},
    },
    binary_op: {
      Binary_op_mult: {}, Binary_op_div: {}, Binary_op_mod: {}, Binary_op_add: {},
      Binary_op_sub: {}, Binary_op_left_shift: {}, Binary_op_right_shift: {},
      Binary_op_unsigned_right_shift: {}, Binary_op_lt: {}, Binary_op_gt: {}, Binary_op_le: {},
      Binary_op_ge: {}, Binary_op_instanceof: {}, Binary_op_in: {}, Binary_op_equal: {},
      Binary_op_disequal: {}, Binary_op_strict_equal: {}, Binary_op_strict_disequal: {},
      Binary_op_bitwise_and: {}, Binary_op_bitwise_or: {}, Binary_op_bitwise_xor: {}, Binary_op_and: {},
      Binary_op_or: {}, Binary_op_coma: {},
    },
    literal: {
      Literal_null: {},
      Literal_bool : {value: "boolean"},
      Literal_number : {value: "number"},
      Literal_string : {value: "string"},
    },
    label: {
      Label_empty: {},
      Label_string : {value: "string"},
    },
    label_set: "label list",
    strictness_flag: "boolean",
    propname: {
      Propname_identifier : {value: "string"},
      Propname_string : {value: "string"},
      Propname_number : {value: "number"},
    },
    expr: {
      Expr_this: {},
      Expr_identifier : {name: "string"},
      Expr_literal : {value: "literal"},
      Expr_object : {fields: "(propname * propbody) list"},
      Expr_array : {elements: "expr option list"},
      Expr_function : {func_name_opt: "string option", arg_names: "string list", body: "funcbody"},
      Expr_access : {obj: "expr", field: "expr"},
      Expr_member : {obj: "expr", field_name: "string"},
      Expr_new : {func: "expr", args: "expr list"},
      Expr_call : {func: "expr", args: "expr list"},
      Expr_unary_op : {op: "unary_op", arg: "expr"},
      Expr_binary_op : {arg1: "expr", op: "binary_op", arg2: "expr"},
      Expr_conditional : {cond: "expr", then_branch: "expr", else_branch: "expr"},
      Expr_assign : {left_expr: "expr", op_opt: "binary_op option", right_expr: "expr"},
    },
    propbody: {
      Propbody_val : {expr: "expr"},
      Propbody_get : {body: "funcbody"},
      Propbody_set : {names: "string list", body: "funcbody"},
    },
    funcbody: {
      Funcbody_intro : {prog: "prog", source: "string"},
    },
    stat: {
      Stat_expr : {expr: "expr"},
      Stat_label : {label: "string", stat: "stat"},
      Stat_block : {stats: "stat list"},
      Stat_var_decl : {decls: "(string * expr option) list"},
      Stat_if : {cond: "expr", then_branch: "stat", else_branch: "stat option"},
      Stat_do_while : {labels: "label_set", body: "stat", cond: "expr"},
      Stat_while : {labels: "label_set", cond: "expr", body: "stat"},
      Stat_with : {obj: "expr", stat: "stat"},
      Stat_throw : {arg: "expr"},
      Stat_return : {arg_opt: "expr option"},
      Stat_break : {label: "label"},
      Stat_continue : {label: "label"},
      Stat_try : {body: "stat", catch_stats_opt: "(string * stat) option", finally_opt: "stat option"},
      Stat_for : {labels: "label_set", init: "expr option", cond: "expr option", step: "expr option", body: "stat"},
      Stat_for_var : {labels: "label_set", init: "(string * expr option) list", cond: "expr option", step: "expr option", body: "stat"},
      Stat_for_in : {labels: "label_set", id: "expr", obj: "expr", body: "stat"},
      Stat_for_in_var : {labels: "label_set", id: "string", init: "expr option", obj: "expr", body: "stat"},
      Stat_debugger: {},
      Stat_switch : {labels: "label_set", arg: "expr", body: "switchbody"},
    },
    switchbody: {
      Switchbody_nodefault : {clauses: "switchclause list"},
      Switchbody_withdefault : {clauses_before: "switchclause list", clause_default: "stat list", clauses_after: "switchclause list"},
    },
    switchclause: {
      Switchclause_intro : {arg: "expr", stats: "stat list"},
    },
    prog: {
      Prog_intro : {strictness: "strictness_flag", elements: "element list"},
    },
    element: {
      Element_stat : {stat: "stat"},
      Element_func_decl : {func_name: "string", arg_names: "string list", body: "funcbody"},
    },
    elements: "element list",
    /* funcdecl: { { funcdecl_name : string;
       funcdecl_parameters : string list;
       funcdecl_body : funcbody } */
  };

  var isBaseType = function(type) {
    return type === "string" ||
           type === "number" ||
           type === "boolean";
  };

  // Returns a base type name, or an object containing the type's constructors
  var getType = function(type) {
    if (isBaseType(type)) {
      return type;
    }

    // Test for tuple types
    if (type.endsWith(')')) {
      type = type.substring(1, type.length - 1);
      return type.split('*').map(s => s.trim());
    }

    // Test for poly type
    var i = type.lastIndexOf(' ');
    if (i >= 0) {
      var polyTypeName = type.substring(i+1);
      var polyTypeConstr = getType(polyTypeName);
      assert(typeof polyTypeConstr === 'function');
      var instance = polyTypeConstr(type.substring(0, i));
      instance._typeName = polyTypeName;
      return instance;
    }

    assert(types.hasOwnProperty(type), "Type " + type + " not present in type environment");
    var t = types[type];
    if (typeof t === "string") {
      // type alias, recursively lookup
      return getType(t);
    } else {
      t._typeName = type;
      return t;
    }
  };

  var typecheck = function(type, value) {
    var t = getType(type);
    if (isBaseType(t)) {
      assert(t === typeof value, errorMsg(value, "was expected to have type of "+t));
    } else if (Array.isArray(t)) {
      assert(Array.isArray(value));
      t.forEach((type, index) => typecheck(type, value[index]));
    } else {
      assert(value.type === t._typeName, errorMsg(value, "was expected to have type of " + t._typeName));
      assert.notEqual(value.tag, "_typeName");
      assert(t.hasOwnProperty(value.tag), value.tag + " is a not a valid constructor of " + t._typeName);

      // Test each field defined in the type constructor
      var constructor = t[value.tag];
      Object.keys(constructor).forEach(field => {
        assert(value.hasOwnProperty(field), errorMsg(value, "doesn't have a " + field + " property"));
        typecheck(constructor[field], value[field]);
      });
    }
  };

  var errorMsg = function (value, msg) {
    return _ => esprimaToAST.toString(value, 3) + " " + msg;
  };

  return typecheck("prog", ast);
}

describe("EsprimaToAST", function() {
  it("Extracts function body strings?", function() {
    var source =
`function f() {
  // body line 1
a()};`;

    var prog = esprima.parse(source, {loc: true, range: true});
    var ast = esprimaToAST.esprimaToAST(prog, source);

    var sourceBody = /{([^]*)}/.exec(source)[1];
    assert.equal(sourceBody, ast.elements.head.body.source);

  });

  [ { source: `"use strict";`                 , strict: true }
  , { source: `'use strict';`                 , strict: true }
  , { source: `"not strict"; "use strict";`   , strict: true }
  , { source: `""; 0; "use strict";`          , strict: false }
  , { source: `"use\\u0020strict";`           , strict: false }
  ].forEach(function (test) {
    it("Correctly parses that `" + test.source + "` is " + (test.strict ? " " : "not ") + "strict mode code?", function() {
      var est = esprima.parse(test.source, {loc: true, range: true});
      var ast = esprimaToAST.esprimaToAST(est, test.source);
      assert.equal(test.strict, ast.strictness);
    });
  });

  var isFunction = function(stat) {
    return stat.tag === "Element_func_decl"
      || (stat.tag === "Element_stat" &&
          stat.stat.tag === "Stat_expr" &&
          stat.stat.expr.tag === "Expr_function");
  };


  var getFunctionStrictness = function(stat) {
    if (stat.tag === "Element_func_decl") {
      return stat.body.prog.strictness;
    } else if (stat.tag === "Element_stat") {
      return stat.stat.expr.body.prog.strictness;
    } else {
      assert.fail();
    }
  };

  [ { source: `"use strict"; function x() {}`                , strict: [true] }
  , { source: `'use strict'; (function() {})`                , strict: [true] }
  , { source: `function x() {"use strict";}`                 , strict: [true] }
  , { source: `(function() {"use strict";})`                 , strict: [true] }
  , { source: `(function(){}); function x(){"use strict";};` , strict: [false, true] }
  ].forEach(function (test) {
    it("Correctly parses that `" + test.source + "` is " + (test.strict ? " " : "not ") + "strict mode code.", function() {
      var est = esprima.parse(test.source, {loc: true, range: true});
      var ast = esprimaToAST.esprimaToAST(est, test.source);

      var listElem = ast.elements;
      var index = 0;
      while (listElem.tag === "::") {
        if (isFunction(listElem.head)) {
          assert.equal(test.strict[index], getFunctionStrictness(listElem.head));
          index++;
        }
        listElem = listElem.tail;
      }
    });
  });
});

test262.addTest(getTest => {
  it('parses?', function() {
    const test = getTest(); // This line cannot be lifted out of the 'it' callback

    var negative = isParserNegativeTest(test.attrs.negative);
    try {
      esprima.parse(test.contents, {loc: true, range: true});
    } catch(e) {
      if (!negative) {
        throw e;
      }
    }
  });

  it('converts ast?', function() {
    const test = getTest();

    try {
      var prog = esprima.parse(test.contents, {loc: true, range: true});
    } catch(e) { this.skip(); }

    try {
      var ast = esprimaToAST.esprimaToAST(prog, test.contents);
      typecheckAST(ast);
    } catch (e) {
      if (e instanceof esprimaToAST.UnsupportedSyntaxError) {
      } else {
        throw e;
      }
    }
  });
});
