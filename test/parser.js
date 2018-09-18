"use strict";

const assert = require('assert');
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
      Coq_unary_op_delete: {}, Coq_unary_op_void: {}, Coq_unary_op_typeof: {}, Coq_unary_op_post_incr: {},
      Coq_unary_op_post_decr: {}, Coq_unary_op_pre_incr: {}, Coq_unary_op_pre_decr: {}, Coq_unary_op_add: {},
      Coq_unary_op_neg: {}, Coq_unary_op_bitwise_not: {}, Coq_unary_op_not: {},
    },
    binary_op: {
      Coq_binary_op_mult: {}, Coq_binary_op_div: {}, Coq_binary_op_mod: {}, Coq_binary_op_add: {},
      Coq_binary_op_sub: {}, Coq_binary_op_left_shift: {}, Coq_binary_op_right_shift: {},
      Coq_binary_op_unsigned_right_shift: {}, Coq_binary_op_lt: {}, Coq_binary_op_gt: {}, Coq_binary_op_le: {},
      Coq_binary_op_ge: {}, Coq_binary_op_instanceof: {}, Coq_binary_op_in: {}, Coq_binary_op_equal: {},
      Coq_binary_op_disequal: {}, Coq_binary_op_strict_equal: {}, Coq_binary_op_strict_disequal: {},
      Coq_binary_op_bitwise_and: {}, Coq_binary_op_bitwise_or: {}, Coq_binary_op_bitwise_xor: {}, Coq_binary_op_and: {},
      Coq_binary_op_or: {}, Coq_binary_op_coma: {},
    },
    literal: {
      Coq_literal_null: {},
      Coq_literal_bool : {value: "boolean"},
      Coq_literal_number : {value: "number"},
      Coq_literal_string : {value: "string"},
    },
    label: {
      Coq_label_empty: {},
      Coq_label_string : {value: "string"},
    },
    label_set: "label list",
    strictness_flag: "boolean",
    propname: {
      Coq_propname_identifier : {value: "string"},
      Coq_propname_string : {value: "string"},
      Coq_propname_number : {value: "number"},
    },
    expr: {
      Coq_expr_this: {},
      Coq_expr_identifier : {name: "string"},
      Coq_expr_literal : {value: "literal"},
      Coq_expr_object : {fields: "(propname * propbody) list"},
      Coq_expr_array : {elements: "expr option list"},
      Coq_expr_function : {func_name_opt: "string option", arg_names: "string list", body: "funcbody"},
      Coq_expr_access : {obj: "expr", field: "expr"},
      Coq_expr_member : {obj: "expr", field_name: "string"},
      Coq_expr_new : {func: "expr", args: "expr list"},
      Coq_expr_call : {func: "expr", args: "expr list"},
      Coq_expr_unary_op : {op: "unary_op", arg: "expr"},
      Coq_expr_binary_op : {arg1: "expr", op: "binary_op", arg2: "expr"},
      Coq_expr_conditional : {cond: "expr", then_branch: "expr", else_branch: "expr"},
      Coq_expr_assign : {left_expr: "expr", op_opt: "binary_op option", right_expr: "expr"},
    },
    propbody: {
      Coq_propbody_val : {expr: "expr"},
      Coq_propbody_get : {body: "funcbody"},
      Coq_propbody_set : {names: "string list", body: "funcbody"},
    },
    funcbody: {
      Coq_funcbody_intro : {prog: "prog", source: "string"},
    },
    stat: {
      Coq_stat_expr : {expr: "expr"},
      Coq_stat_label : {label: "string", stat: "stat"},
      Coq_stat_block : {stats: "stat list"},
      Coq_stat_var_decl : {decls: "(string * expr option) list"},
      Coq_stat_if : {cond: "expr", then_branch: "stat", else_branch: "stat option"},
      Coq_stat_do_while : {labels: "label_set", body: "stat", cond: "expr"},
      Coq_stat_while : {labels: "label_set", cond: "expr", body: "stat"},
      Coq_stat_with : {obj: "expr", stat: "stat"},
      Coq_stat_throw : {arg: "expr"},
      Coq_stat_return : {arg_opt: "expr option"},
      Coq_stat_break : {label: "label"},
      Coq_stat_continue : {label: "label"},
      Coq_stat_try : {body: "stat", catch_stats_opt: "(string * stat) option", finally_opt: "stat option"},
      Coq_stat_for : {labels: "label_set", init: "expr option", cond: "expr option", step: "expr option", body: "stat"},
      Coq_stat_for_var : {labels: "label_set", init: "(string * expr option) list", cond: "expr option", step: "expr option", body: "stat"},
      Coq_stat_for_in : {labels: "label_set", id: "expr", obj: "expr", body: "stat"},
      Coq_stat_for_in_var : {labels: "label_set", id: "string", init: "expr option", obj: "expr", body: "stat"},
      Coq_stat_debugger: {},
      Coq_stat_switch : {labels: "label_set", arg: "expr", body: "switchbody"},
    },
    switchbody: {
      Coq_switchbody_nodefault : {clauses: "switchclause list"},
      Coq_switchbody_withdefault : {clauses_before: "switchclause list", clause_default: "stat list", clauses_after: "switchclause list"},
    },
    switchclause: {
      Coq_switchclause_intro : {arg: "expr", stats: "stat list"},
    },
    prog: {
      Coq_prog_intro : {strictness: "strictness_flag", elements: "element list"},
    },
    element: {
      Coq_element_stat : {stat: "stat"},
      Coq_element_func_decl : {func_name: "string", arg_names: "string list", body: "funcbody"},
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
      assert.strictEqual(typeof polyTypeConstr, 'function');
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
      assert.strictEqual(typeof value, t, errorMsg(value, "was expected to have type of "+t));
    } else if (Array.isArray(t)) {
      assert(Array.isArray(value), errorMsg(value, "was expected to be an array"));
      t.forEach((type, index) => typecheck(type, value[index]));
    } else {
      assert.strictEqual(t._typeName, value.type, errorMsg(value, "was expected to have type of " + t._typeName));
      assert.notEqual("_typeName", value.tag);
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
    assert.strictEqual(sourceBody, ast.elements.head.body.source);

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
      assert.strictEqual(test.strict, ast.strictness);
    });
  });

  var isFunction = function(stat) {
    return stat.tag === "Coq_element_func_decl"
      || (stat.tag === "Coq_element_stat" &&
          stat.stat.tag === "Coq_stat_expr" &&
          stat.stat.expr.tag === "Coq_expr_function");
  };


  var getFunctionStrictness = function(stat) {
    if (stat.tag === "Coq_element_func_decl") {
      return stat.body.prog.strictness;
    } else if (stat.tag === "Coq_element_stat") {
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
          assert.strictEqual(test.strict[index], getFunctionStrictness(listElem.head));
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
