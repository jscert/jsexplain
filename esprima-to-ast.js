"use strict";

//https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API$revision/707671

function esprimaToAST(prog) {
  var toList = function (array) {
    var r = {type: "list", tag: "[]"};
    for (var i = array.length - 1; i >= 0; i--) {
      r = {type: "list", tag: "::", head: array[i], tail: r};
    }
    return r;
  };

  var toLoc = function (pos) {
    if (pos === null) {throw "null position in esprima AST";};
    return {file: "input.js",
            start: {line: pos.start.line, col: pos.start.column},
            stop:  {line: pos.end.line, col: pos.end.column}};
  };

  var id = function (i) { return i; }

  var toOption = function (funcTr, node) {
    var option = { type: "option" };
    if (node === null) {
      option.tag = "None";
    } else {
      option.tag = "Some";
      option.value = funcTr(node);
    }
    return option;
  };

  // [] => None, [a] => Some a
  var arrayToOption = function (funcTr, arr) {
    if (arr.length > 1) {
      throw new EsprimaToASTError("arrayToOption error, arr.length > 1.", arr);
    }
    return toOption(funcTr, (arr.length > 0 ? arr[0] : null));
  }

  /*** Miscellaneous Helper Nodes ***/

  // The JSCert AST special-cases this instance of option
  var trOptLabel = function (label) {
    var option = { type: "label" };
    if (label === null) {
      option.tag = "Coq_label_empty";
    } else {
      option.tag = "Coq_label_string";
      option.value = label;
    }
    return option;
  };


  /*** Program ***/

  var trProg = function (prog) {
    if (prog.type !== "Program") {
      throw new EsprimaToASTError("trProg error: " + prog.type, prog);
    };
    var r = {loc: toLoc(prog.loc), type: "prog"};
    r.tag = "Coq_prog_intro";
    // TODO deal with strictness
    r.strictness = false;
    r.elements = toList(prog.body.map(trStatAsElement));
    return r;
  };

  /*** Statements ***/

  var trStatAsElement = function (stat) {
    var loc = toLoc(stat.loc);
    var r = {loc: loc, type: "element"};
    if (stat.type === "FunctionDeclaration") {
      r.tag = "Coq_element_func_decl";
      r.func_name = trIdentifier(stat.id);
      r.arg_names = trParams(stat.params);
      // TODO this could be detected by stat.expression of type bool
      r.body = trBlockStatAsFuncbody(stat.body);
    } else /* if (stat.type <: "Statement") */  {
      r.tag = "Coq_element_stat";
      r.stat = trStat(stat);
    } // else {
    //   throw new EsprimaToASTError("trStatAsElement error: " + stat.type, stat);
    // }
    return r;
  };

  var trBlockStatAsFuncbody = function (stat) {
      if (stat.type !== "BlockStatement") {
        throw new EsprimaToASTError("stat.type !== BlockStatement", stat);
      }

      var loc = toLoc(stat.loc);

      // TODO strictness
      var prog = {loc: loc, type: "prog", tag: "Coq_prog_intro",
                  strictness: false,
                  elements: toList(stat.body.map(trStatAsElement))};
      // TODO we don't know how to get source
      return {loc: loc, type: "funcbody", tag: "Coq_funcbody_intro",
                prog: prog, source: ""};
  };

  var trFuncExprAsFuncbody = function (expr) {
      if (expr.type !== "FunctionExpression") {
        throw new EsprimaToASTError("expr.type !== FunctionExpression", expr);
      }
      checkFuncExpr(expr);
      return trBlockStatAsFuncbody(expr.body);
  };


  var trParams = function (params) {
    return toList(params.map(trPattern));
  }

  // return string until ES6 support is required
  var trPattern = function (pat) {
    if (pat.type === "Identifier") {
      return trIdentifier(pat);
    } else {
      throw new EsprimaToASTError("trPattern error: " + pat.type, pat);
    }
  };

  var trIdentifier = function (ident) {
    if (ident.type === "Identifier") {
      return ident.name;
    } else {
      throw new EsprimaToASTError("Expected Identifier, got: " + ident.type);
    }
  }

  // return the encoding of a pair
  var trVarDecl = function (decl) {
    if (decl.type !== "VariableDeclarator") {
      throw new ExprimaToASTError("trVarDecl called with wrong type: " + decl.type);
    }
    var id = trPattern(decl.id);
    var eo = toOption(trExpr, decl.init);
    return [id,eo];
  };

  var trProperty = function (property) {
    if (property.type !== "Property") {
      throw new EsprimaToASTError("trProperty called with wrong type: " + property.type);
    }

    var name;
    if (property.key.type === "Literal") {
      name = property.key.value; // value cannot be null due to parse tree
    } else if (property.key.type === "Identifier") {
      name = trIdentifier(property.key);
    } else {
      throw new EsprimaToASTError("trProperty called with wrong identifier type: " + property.type);
    }

    var propbody = { loc: toLoc(property.loc), type: "propbody" }
    if (property.kind === "init") {
      propbody.tag = "Coq_propbody_val";
      propbody.expr = trExpr(property.value);
    } else if (property.kind === "get") {
      propbody.tag = "Coq_propbody_get";
      propbody.body = trFuncExprAsFuncbody(property.value);
    } else if (property.kind === "set") {
      propbody.tag = "Coq_propbody_get";
      propbody.body = trFuncExprAsFuncbody(property.value);
      propbody.names = toList(property.value.params.map(trPattern));
    } else {
      throw new EsprimaToASTError("trProperty got unexpected kind: " + property.kind);
    }

    return [name, propbody];
  };

  var trCatchClause = function (clause) {
    if (clause.type !== "CatchClause") {
      throw new EsprimaToASTError("trCatchClause called with wrong type: " + clause.type);
    }
    // clause.guard is a custom Mozilla extension
    return [clause.param, trBlockStat(clause.body)];
  };

  /*** Statements ***/

  var trBlockStat = function (stat) {
    var r = {loc: toLoc(stat.loc), type: "stat"};
    if (stat.type === "BlockStatement") {
      r.tag = "Coq_stat_block";
      r.stats = toList(stat.body.map(trStat));
    } else {
      throw new EsprimaToASTError("trStat error: " + stat.type, stat);
    };
    return r;
  };

  var trStat = function (stat) {
    var r = {loc: toLoc(stat.loc), type: "stat"};
    if (stat.type === "EmptyStatement") {
      r.tag = "Coq_stat_block";
      r.stats = toList([]);
    } else if (stat.type === "BlockStatement") {
      r = trBlockStat(stat);
    } else if (stat.type === "ExpressionStatement") {
      r.tag = "Coq_stat_expr";
      r.expr = trExpr(stat.expression);
    } else if (stat.type === "IfStatement") {
      r.tag = "Coq_stat_if";
      r.cond = trExpr(stat.test);
      r.then_branch = trStat(stat.consequent);
      r.else_branch = toOption(trStat, stat.alternate);
    } else if (stat.type === "LabeledStatement") {
      r.tag = "Coq_stat_label";
      r.label = stat.label;
      r.stat = trStat(stat.body);
    } else if (stat.type === "BreakStatement") {
      r.tag = "Coq_stat_break";
      r.label = trOptLabel(stat.label);
    } else if (stat.type === "ContinueStatement") {
      r.tag = "Coq_stat_continue";
      r.label = trOptLabel(stat.label);
    } else if (stat.type === "WithStatement") {
      r.tag = "Coq_stat_with";
      r.obj = trExpr(stat.object);
      r.stat = trStat(stat.body);
    } else if (stat.type === "SwitchStatement") {
      throw new EsprimaToASTError("switch");
      r.tag = "Coq_stat_switch";
      r.arg = trExpr(stat.discriminant);
    } else if (stat.type === "ReturnStatement") {
      r.tag = "Coq_stat_return";
      r.arg_opt = toOption(trExpr, stat.argument);
    } else if (stat.type === "ThrowStatement") {
      r.tag = "Coq_stat_throw";
      r.arg = trExpr(stat.argument);
    } else if (stat.type === "TryStatement") {
      r.tag = "Coq_stat_try";
      r.body = trStat(stat.block);
      // NOTE: Esprima v1.2.5 deviates from the SpiderMonkey AST standard here:
      // handler: CatchClause | null;
      // guardedHandlers: [ CatchClause ];          // Mozilla extension
      // r.catch_stats_opt = toOption(trCatchClause, stat.handler);
      r.catch_stats_opt = arrayToOption(trCatchClause, stat.handlers);
      r.finally_opt = toOption(trBlockStat, stat.finalizer);
    } else if (stat.type === "WhileStatement") {
      r.tag = "Coq_stat_while";
      r.cond = trExpr(stat.test);
      r.body = trStat(stat.body);

      // This is precisely what interp/src/translate_syntax.ml does...
      r.labels = toList([]);
    } else if (stat.type === "DoWhileStatement") {
      r.tag = "Coq_stat_do_while";
      r.cond = trExpr(stat.test);
      r.body = trStat(stat.body);

      // This is precisely what interp/src/translate_syntax.ml does...
      r.labels = toList([]);
    } else if (stat.type === "ForStatement") {
      if (stat.init && stat.init.type === "VariableDeclaration") {
        r.tag = "Coq_stat_for_var";
        r.init = trStat(stat.init).decls;
      } else {
        r.tag = "Coq_stat_for";
        r.init = toOption(trExpr, stat.init);
      }
      r.cond = toOption(trExpr, stat.test);
      r.step = toOption(trExpr, stat.update);
      r.body = trStat(stat.body);

      // This is precisely what interp/src/translate_syntax.ml does...
      r.labels = toList([]);
    } else if (stat.type === "ForInStatement") {
      throw new UnsupportedSyntaxError("We don't support for-in.", stat);

      // Everything below here is for when we do support for-in ;P

      if (stat.left.type === "VariableDeclaration") {
        r.tag = "Coq_stat_for_in_var";
        if (stat.left.declarations.length != 1) {
          throw new EsprimaToASTError("ForInStatement: Wrong number of declarations.", stat);
        }
        // TODO: Technically, the declaration should be executed, but this
        // doesn't match our (current, broken) JsSyntax type signature
        r.id = trStat(stat.left).decls.head[0];
      } else {
        r.tag = "Coq_stat_for_in";
        r.id = trExpr(stat.left);
      }
      r.obj = trExpr(stat.right);
      r.body = trStat(stat.body);
      r.labels = toList([]);

    } else if (stat.type === "VariableDeclaration") {
      r.tag = "Coq_stat_var_decl";
      if ("kind" in stat && stat.kind !== "var") {
        throw new UnsupportedSyntaxError("Only var bindings are supported.", stat);
      }
      r.decls = toList(stat.declarations.map(trVarDecl));
    } else if (stat.type === "FunctionDeclaration") {
      throw new UnsupportedSyntaxError("Function declaration in wrong place.", stat);
    } else {
      throw new EsprimaToASTError("trStat error: " + stat.type, stat);
    };
    return r;
  };

  /*** Expressions ***/

  var trExprAsLiteral = function (expr) {
    var r = {loc: toLoc(expr.loc), type: "literal"};
    var v = expr.value;
    var t = typeof(v);
    if (t === "null") {
      r.tag = "Coq_literal_null";
    } else if (t === "boolean") {
      r.tag = "Coq_literal_bool";
      r.value = v;
    } else if (t === "number") {
      r.tag = "Coq_literal_number";
      r.value = v;
    } else if (t === "string") {
      r.tag = "Coq_literal_string";
      r.value = v;
    } else if (t === "object") {
      throw new UnsupportedSyntaxError("Unsupported literal: " + expr.raw, expr);
    } else {
      throw new EsprimaToASTError("trExprAsLiteral error: " + t, expr);
    };
    return r;
  };

  var trExpr = function (expr) {
    var r = {loc: toLoc(expr.loc), type: "expr"};
    if (expr.type === "ThisExpression") {
      r.tag = "Coq_expr_this";
    } else if (expr.type === "ArrayExpression") {
      r.tag = "Coq_expr_array";
      r.elements = toList(expr.elements.map(toOption.bind(null, trExpr)));
    } else if (expr.type === "ObjectExpression") {
      r.tag = "Coq_expr_object";
      r.fields = toList(expr.properties.map(trProperty));
    } else if (expr.type === "FunctionExpression") {
      checkFuncExpr(expr);
      r.tag = "Coq_expr_function";
      r.func_name_opt = toOption(id, expr.id);
      r.arg_names = toList(expr.params.map(trPattern));
      r.body = trBlockStat(expr.body);
      // TODO: USE STRICT
    } else if (expr.type === "SequenceExpression") {
      r = expr.expressions.map(trExpr).reduce(function (previousValue, currentValue) {
        return {
          type: "expr",
          tag: "Coq_expr_binary_op",
          arg1: previousValue,
          op: { type: "binary_op", tag: "Cow_binary_op_coma" },
          arg2: currentValue,
          loc: {
            file: previousValue.loc.file,
            start: previousValue.loc.start,
            stop: currentValue.loc.stop
          }
        };
      });
    } else if (expr.type === "UnaryExpression") {
      r.tag = "Coq_expr_unary_op";
      r.op = trUnaryOp(expr.operator);
      // expr.prefix (boolean) unused
      r.arg = trExpr(expr.argument);
    } else if (expr.type === "BinaryExpression") {
      r.tag = "Coq_expr_binary_op";
      r.arg1 = trExpr(expr.left);
      r.op = trBinaryOp(expr.operator);
      r.arg2 = trExpr(expr.right);
    } else if (expr.type === "AssignmentExpression") {
      r.tag = "Coq_expr_assign";
      r.left_expr = trExpr(expr.left);
      r.op_opt = trAssignmentOp(expr.operator);
      r.right_expr = trExpr(expr.right);
    } else if (expr.type === "UpdateExpression") {
      r.tag = "Coq_expr_unary_op";
      r.op = trUpdateOp(expr.operator, expr.prefix);
      r.arg = trExpr(expr.argument);
    } else if (expr.type === "LogicalExpression") {
      r.tag = "Coq_expr_binary_op";
      r.arg1 = trExpr(expr.left);
      r.op = trLogicalOp(expr.operator);
      r.arg2 = trExpr(expr.right);
    } else if (expr.type === "ConditionalExpression") {
      r.tag = "Coq_expr_conditional";
      r.cond = trExpr(expr.test);
      r.then_branch = trExpr(expr.consequent);
      r.else_branch = trExpr(expr.alternate);
    } else if (expr.type === "NewExpression") {
      r.tag = "Coq_expr_new";
      r.func = trExpr(expr.callee);
      r.args = toList(expr.arguments.map(trExpr));
    } else if (expr.type === "CallExpression") {
      r.tag = "Coq_expr_call";
      r.func = trExpr(expr.callee);
      r.args = toList(expr.arguments.map(trExpr));
    } else if (expr.type === "MemberExpression" && !expr.computed) {
      r.tag = "Coq_expr_member";
      r.obj = trExpr(expr.object);
      r.field_name = trIdentifier(expr.property);
    } else if (expr.type === "MemberExpression" && expr.computed) {
      r.tag = "Coq_expr_access";
      r.obj = trExpr(expr.object);
      r.field = trExpr(expr.property);
    } else if (expr.type === "Identifier") {
      r.tag = "Coq_expr_identifier";
      r.name = trIdentifier(expr);
    } else if (expr.type === "Literal") {
      r.tag = "Coq_expr_literal";
      r.value = trExprAsLiteral(expr);
    } else {
      throw new EsprimaToASTError("trExpr error: " + expr.type, expr);
    };
    return r;
  };

  var checkFuncExpr = function (expr) {
      // Unsupported (ES6?) Syntax Checking
      if (expr.generator || expr.expression || expr.rest || expr.defaults.length > 0) {
        throw new UnsupportedSyntaxError("Function Expression", expr);
      }
  };

  /*** Operators ***/

  const unary_operators = Object.create(null);
  unary_operators["-"] = "Coq_unary_op_neg";
  unary_operators["+"] = "Coq_unary_op_add";
  unary_operators["!"] = "Coq_unary_op_not";
  unary_operators["~"] = "Coq_unary_op_bitwise_not";
  unary_operators["void"] = "Coq_unary_op_void";
  unary_operators["typeof"] = "Coq_unary_op_typeof";
  unary_operators["delete"] = "Coq_unary_op_delete";
  var trUnaryOp = function (op) {
    return { type: "unary_op", tag: unary_operators[op] };
  };

  var binaryOpTagToObj = function (tag) {
    return { type: "binary_op", tag: tag };
  };

  const binary_operators = Object.create(null);
  binary_operators["=="] = "Coq_binary_op_equal";
  binary_operators["!="] = "Coq_binary_op_disequal";
  binary_operators["==="] = "Coq_binary_op_strict_equal";
  binary_operators["!=="] = "Coq_binary_op_strict_disequal";
  binary_operators["<"] = "Coq_binary_op_lt";
  binary_operators["<="] = "Coq_binary_op_le";
  binary_operators[">"] = "Coq_binary_op_gt";
  binary_operators[">="] = "Coq_binary_op_ge";
  binary_operators["<<"] = "Coq_binary_op_left_shift";
  binary_operators[">>"] = "Coq_binary_op_right_shift";
  binary_operators[">>>"] = "Coq_binary_op_unsigned_right_shift";
  binary_operators["+"] = "Coq_binary_op_add";
  binary_operators["-"] = "Coq_binary_op_sub";
  binary_operators["*"] = "Coq_binary_op_mult";
  binary_operators["/"] = "Coq_binary_op_div";
  binary_operators["%"] = "Coq_binary_op_mod";
  binary_operators["|"] = "Coq_binary_op_bitwise_or";
  binary_operators["^"] = "Coq_binary_op_bitwise_xor";
  binary_operators["&"] = "Coq_binary_op_bitwise_and";
  binary_operators["in"] = "Coq_binary_op_in";
  binary_operators["instanceof"] = "Coq_binary_op_instanceof";
  var trBinaryOp = function (op) {
    return binaryOpTagToObj(binary_operators[op]);
  };

  const assignment_operators = Object.create(null);
  assignment_operators["="] = null;
  assignment_operators["+="] = "Coq_binary_op_add";
  assignment_operators["-="] = "Coq_binary_op_sub";
  assignment_operators["*="] = "Coq_binary_op_mult";
  assignment_operators["/="] = "Coq_binary_op_div";
  assignment_operators["%="] = "Coq_binary_op_mod";
  assignment_operators["<<="] = "Coq_binary_op_left_shift";
  assignment_operators[">>="] = "Coq_binary_op_right_shift";
  assignment_operators[">>>="] = "Coq_binary_op_unsigned_right_shift";
  assignment_operators["|="] = "Coq_binary_op_bitwise_or";
  assignment_operators["^="] = "Coq_binary_op_bitwise_xor";
  assignment_operators["&="] = "Coq_binary_op_bitwise_and";
  var trAssignmentOp = function (op) {
    return toOption(binaryOpTagToObj, assignment_operators[op]);
  };

  const update_operators = Object.create(null);
  update_operators["++"] = "incr";
  update_operators["--"] = "decr";
  var trUpdateOp = function (op, prefix) {
    var fix = prefix ? "pre" : "post";
    return {
      type: "unary_op",
      tag: "Coq_unary_op_" + fix + "_" + update_operators[op]
    };
  };

  const logical_operators = Object.create(null);
  logical_operators["||"] = "Coq_binary_op_or";
  logical_operators["&&"] = "Coq_binary_op_and";
  var trLogicalOp = function (op) {
    return { type: "binary_op", tag: logical_operators[op] };
  };

  return trProg(prog);
}


/* Custom Error handler with JSON pretty-printing */

function toString(ast, maxDepth) {
  if (maxDepth === undefined) { maxDepth = 3; }

  return JSON.stringify(ast, function ASTErrorJSONReplacer(key, value, depth) {
    if (depth === undefined) { depth = maxDepth; }

    if (typeof value === "object") {
      if (depth > 0) {
        for (var nestKey in value) {
          value[nestKey] = ASTErrorJSONReplacer(nestKey, value[nestKey], depth - 1);
        }
      } else {
        value = "RECURSION TRUNCATED";
      }
    }

    return value;
  }, 2);
}

function NewASTErrorType(name, parentError) {
  var error = function (message, expr) {
    this.name = name;
    this.message = message + "\n" + toString(expr);
    if (Error.captureStackTrace) { Error.captureStackTrace(this, error); }
  }
  error.prototype = Object.create(parentError.prototype);
  error.prototype.constructor = error;
  return error;
}
var EsprimaToASTError = NewASTErrorType("EsprimaToASTError", Error);
var UnsupportedSyntaxError = NewASTErrorType("UnsupportedSyntaxError", EsprimaToASTError);

// For testing purposes with node
module.exports.esprimaToAST = esprimaToAST;
module.exports.EsprimaToASTError = EsprimaToASTError;
module.exports.UnsupportedSyntaxError = UnsupportedSyntaxError;
module.exports.toString = toString;
