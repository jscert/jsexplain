"use strict";

var fs = require('mz/fs');
var walk = require('klaw');
var filter = require('through2-filter');
fs.readlinkSync = require('readlink').sync; // a non-broken readlink...
var assert = require('chai').assert;

var esprima = require('esprima');
var esprimaToAST = require('../esprima-to-ast.js');

var test262path = fs.readlinkSync(__dirname + '/test262');
var tests = [];

/* Tests whether a given test is negative.
 * Returns: a string if type of failure specified, true, or false
 */
function testNegativity(str) {
  var result = /@negative[ \t]*(\S*)?[ \t]*$/m.exec(str);
  if(result) {
    result = result[1] || true;
  } else {
    result = false;
  }
  return result;
}

function isParserNegativeTest(str) {
  var negative = testNegativity(str);
  if (typeof negative === 'boolean') {
    return negative;
  }
  // Second case testing for an Early (Syntax) Error
  return /(?:SyntaxError|\?!NotEarlyError)/.test(negative);
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
      assert.isFunction(polyTypeConstr);
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
    } else if (t instanceof Array) {
      assert.instanceOf(value, Array);
      t.forEach((type, index) => typecheck(type, value[index]));
    } else {
      assert(value.type === t._typeName, errorMsg(value, "was expected to have type of " + t._typeName));
      assert.notStrictEqual(value.tag, "_typeName");
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

walk(test262path)
.pipe(filter.obj(file => file.stats.isFile() && file.path.endsWith(".js")))
.on('readable', function() {
  var item;
  while((item = this.read())) { tests.push(item.path); }
})
.on('end', function() {
  describe("test262", function() {
    tests.forEach(item => {
      describe(item, function() {

        var source;
        var negative = '';

        before(function(done) {
          fs.readFile(item).then(
            data => {
              source = data.toString();
              negative = isParserNegativeTest(source);
            }
          ).then(done);
        });

        it('parses', function() {
          try {
            esprima.parse(source, {loc: true});
          } catch(e) {
            if (!negative) {
              throw e;
            }
          }
        });

        it('converts', function() {
          try {
            var prog = esprima.parse(source, {loc: true});
          } catch(e) { return; }

          try {
            var ast = esprimaToAST.esprimaToAST(prog);
            typecheckAST(ast);
          } catch (e) {
            if (e instanceof esprimaToAST.UnsupportedSyntaxError) {
            } else {
              console.log(JSON.stringify(ast, null, 2));
              throw e;
            }
          }
        });
      })
    });
  });

  run();
});
