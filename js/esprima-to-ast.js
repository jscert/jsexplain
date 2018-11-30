"use strict";

//https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API$revision/707671

function esprimaToAST(prog, sourceText, filename) {
  filename = filename === undefined ? "" : filename;

  var returnSourceText = filename.startsWith("_eval_");

  var contextStrictMode = false;

  var toList = function (array) {
    var r = {type: "list", tag: "[]"};
    for (var i = array.length - 1; i >= 0; i--) {
      r = {type: "list", tag: "::", head: array[i], tail: r};
    }
    return r;
  };

  var toLoc = function (pos) {
    if (pos === null) {throw "null position in esprima AST";};
    // TODO : could reuse the start and end object
    var loc = {file: filename,
               start: {line: pos.start.line, column: pos.start.column},
               end:  {line: pos.end.line, column: pos.end.column},
               sourceText: ""};
    if (returnSourceText) loc.sourceText = sourceText;
    return loc;
  };

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

  var isDirectivePrologue = function(stat) {
    return stat.type === "ExpressionStatement" &&
           stat.expression.type === "Literal" &&
           (typeof stat.expression.value) === "string";
  }

  var isUseStrictDirective = function(stat) {
    return isDirectivePrologue(stat) &&
           /^["']use strict["']$/.test(stat.expression.raw);
  }

  var getDirectivePrologue = function(statArr) {
    // statArr.takeWhile(isDirectivePrologue)
    return statArr.filter(function(stat) {
      this.ok = this.ok && isDirectivePrologue(stat);
      return this.ok;
    }, { ok: true });
  };

  var bodyIsStrict = function(statArr) {
    return getDirectivePrologue(statArr).some(isUseStrictDirective);
  }

  /*** Miscellaneous Helper Nodes ***/

  // The JSCert AST special-cases this instance of option
  var trOptLabel = function (label) {
    var option = { type: "label" };
    if (label === null) {
      option.tag = "Label_empty";
    } else {
      option.tag = "Label_string";
      option.value = trIdentifier(label);
    }
    return option;
  };


  /*** Program ***/

  var trProg = function (prog) {
    if (prog.type !== "Program") {
      throw new EsprimaToASTError("trProg error: " + prog.type, prog);
    };
    var r = {loc: toLoc(prog.loc), type: "prog"};
    r.tag = "Prog_intro";
    r.strictness = contextStrictMode = bodyIsStrict(prog.body);
    r.elements = toList(prog.body.map(trStatAsElement));
    return r;
  };

  /*** Statements ***/

  var trStatAsElement = function (stat) {
    var loc = toLoc(stat.loc);
    var r = {loc: loc, type: "element"};
    if (stat.type === "FunctionDeclaration") {
      r.tag = "Element_func_decl";
      r.func_name = trIdentifier(stat.id);
      r.arg_names = trParams(stat.params);
      // TODO this could be detected by stat.expression of type bool
      r.body = trBlockStatAsFuncbody(stat.body);
    } else /* if (stat.type <: "Statement") */  {
      r.tag = "Element_stat";
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
      var prog = { loc: loc, type: "prog", tag: "Prog_intro" };

      var oldContextStrictMode = contextStrictMode;
      contextStrictMode = contextStrictMode || bodyIsStrict(stat.body);
      prog.strictness = contextStrictMode;
      prog.elements = toList(stat.body.map(trStatAsElement));
      contextStrictMode = oldContextStrictMode;

      var source = "";
      if (sourceText && stat.range) {
        // Esprima's range includes the { }
        source = sourceText.slice(stat.range[0]+1, stat.range[1]-1);
      }

      return {loc: loc, type: "funcbody", tag: "Funcbody_intro",
                prog: prog, source: source};
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

    var propname = { type: "propname", loc: toLoc(property.key.loc) };
    if (property.key.type === "Literal") {
      propname.value = property.key.value;

      if ((typeof propname.value) === "number") {
        propname.tag = "Propname_number";
      } else if ((typeof propname.value) === "string") {
        propname.tag = "Propname_string";
      } else {
        throw new EsprimaToASTError("Property key may only be number or string, got: " + property.key.value);
      }
    } else if (property.key.type === "Identifier") {
      propname.tag = "Propname_identifier";
      propname.value = trIdentifier(property.key);
    } else {
      throw new EsprimaToASTError("trProperty called with wrong identifier type: " + property.type);
    }

    var propbody = { loc: toLoc(property.value.loc), type: "propbody" }
    if (property.kind === "init") {
      propbody.tag = "Propbody_val";
      propbody.expr = trExpr(property.value);
    } else if (property.kind === "get") {
      propbody.tag = "Propbody_get";
      propbody.body = trFuncExprAsFuncbody(property.value);
    } else if (property.kind === "set") {
      propbody.tag = "Propbody_set";
      propbody.body = trFuncExprAsFuncbody(property.value);
      propbody.names = toList(property.value.params.map(trPattern));
    } else {
      throw new EsprimaToASTError("trProperty got unexpected kind: " + property.kind);
    }

    return [propname, propbody];
  };

  var trCatchClause = function (clause) {
    if (clause.type !== "CatchClause") {
      throw new EsprimaToASTError("trCatchClause called with wrong type: " + clause.type);
    }
    // clause.guard is a custom Mozilla extension
    return [trPattern(clause.param), trBlockStat(clause.body)];
  };

  var trSwitchCase = function (scase) {
    if (scase.type !== "SwitchCase") {
      throw new EsprimaToASTError("trSwitchCase called with wrong type: " + clause.type);
    }
    return {
      type: "switchclause",
      tag: "Switchclause_intro",
      loc: toLoc(scase.loc),
      arg: trExpr(scase.test),
      stats: toList(scase.consequent.map(trStat))
    };
  }


  /*** Statements ***/

  var trBlockStat = function (stat) {
    var r = {loc: toLoc(stat.loc), type: "stat"};
    if (stat.type === "BlockStatement") {
      r.tag = "Stat_block";
      r.stats = toList(stat.body.map(trStat));
    } else {
      throw new EsprimaToASTError("trStat error: " + stat.type, stat);
    };
    return r;
  };

  var trStat = function (stat) {
    var r = {loc: toLoc(stat.loc), type: "stat"};
    if (stat.type === "EmptyStatement") {
      r.tag = "Stat_block";
      r.stats = toList([]);
    } else if (stat.type === "BlockStatement") {
      r = trBlockStat(stat);
    } else if (stat.type === "ExpressionStatement") {
      r.tag = "Stat_expr";
      r.expr = trExpr(stat.expression);
    } else if (stat.type === "IfStatement") {
      r.tag = "Stat_if";
      r.cond = trExpr(stat.test);
      r.then_branch = trStat(stat.consequent);
      r.else_branch = toOption(trStat, stat.alternate);
    } else if (stat.type === "LabeledStatement") {
      r.tag = "Stat_label";
      r.label = trIdentifier(stat.label);
      r.stat = trStat(stat.body);
    } else if (stat.type === "BreakStatement") {
      r.tag = "Stat_break";
      r.label = trOptLabel(stat.label);
    } else if (stat.type === "ContinueStatement") {
      r.tag = "Stat_continue";
      r.label = trOptLabel(stat.label);
    } else if (stat.type === "WithStatement") {
      r.tag = "Stat_with";
      r.obj = trExpr(stat.object);
      r.stat = trStat(stat.body);
    } else if (stat.type === "SwitchStatement") {
      r.tag = "Stat_switch";
      r.arg = trExpr(stat.discriminant);
      r.labels = toList([]);
      r.body = { type: "switchbody" };

      // Find the index of the default clause (if any)
      var index = stat.cases.findIndex(clause => clause.test === null);
      if (index >= 0) {
        r.body.tag = "Switchbody_withdefault";
        r.body.clauses_before = toList(stat.cases.slice(0, index).map(trSwitchCase));
        r.body.clause_default = toList(stat.cases[index].consequent.map(trStat));
        r.body.clauses_after  = toList(stat.cases.slice(index+1).map(trSwitchCase));
      } else {
        r.body.tag = "Switchbody_nodefault";
        r.body.clauses = toList(stat.cases.map(trSwitchCase));
      }

    } else if (stat.type === "ReturnStatement") {
      r.tag = "Stat_return";
      r.arg_opt = toOption(trExpr, stat.argument);
    } else if (stat.type === "ThrowStatement") {
      r.tag = "Stat_throw";
      r.arg = trExpr(stat.argument);
    } else if (stat.type === "TryStatement") {
      r.tag = "Stat_try";
      r.body = trStat(stat.block);
      // NOTE: Esprima v1.2.5 deviates from the SpiderMonkey AST standard here:
      // handler: CatchClause | null;
      // guardedHandlers: [ CatchClause ];          // Mozilla extension
      // r.catch_stats_opt = toOption(trCatchClause, stat.handler);
      r.catch_stats_opt = arrayToOption(trCatchClause, stat.handlers);
      r.finally_opt = toOption(trBlockStat, stat.finalizer);
    } else if (stat.type === "WhileStatement") {
      r.tag = "Stat_while";
      r.cond = trExpr(stat.test);
      r.body = trStat(stat.body);

      // This is precisely what interp/src/translate_syntax.ml does...
      r.labels = toList([]);
    } else if (stat.type === "DoWhileStatement") {
      r.tag = "Stat_do_while";
      r.cond = trExpr(stat.test);
      r.body = trStat(stat.body);

      // This is precisely what interp/src/translate_syntax.ml does...
      r.labels = toList([]);
    } else if (stat.type === "ForStatement") {
      if (stat.init && stat.init.type === "VariableDeclaration") {
        r.tag = "Stat_for_var";
        r.init = trStat(stat.init).decls;
      } else {
        r.tag = "Stat_for";
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
        r.tag = "Stat_for_in_var";
        if (stat.left.declarations.length != 1) {
          throw new EsprimaToASTError("ForInStatement: Wrong number of declarations.", stat);
        }
        // TODO: Technically, the declaration should be executed, but this
        // doesn't match our (current, broken) JsSyntax type signature
        r.id = trStat(stat.left).decls.head[0];
      } else {
        r.tag = "Stat_for_in";
        r.id = trExpr(stat.left);
      }
      r.obj = trExpr(stat.right);
      r.body = trStat(stat.body);
      r.labels = toList([]);

    } else if (stat.type === "VariableDeclaration") {
      r.tag = "Stat_var_decl";
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
    if (t === "object" && expr.raw === "null") {
      r.tag = "Literal_null";
    } else if (t === "boolean") {
      r.tag = "Literal_bool";
      r.value = v;
    } else if (t === "number") {
      r.tag = "Literal_number";
      r.value = v;
    } else if (t === "string") {
      r.tag = "Literal_string";
      r.value = v;
    } else if (t === "object" && expr.raw[0] === "/") {
      throw new UnsupportedSyntaxError("Regular Expression literal: " + expr.raw, expr);
    } else {
      throw new EsprimaToASTError("trExprAsLiteral error: " + t, expr);
    };
    return r;
  };

  var trExpr = function (expr) {
    var r = {loc: toLoc(expr.loc), type: "expr"};
    if (expr.type === "ThisExpression") {
      r.tag = "Expr_this";
    } else if (expr.type === "ArrayExpression") {
      r.tag = "Expr_array";
      r.elements = toList(expr.elements.map(toOption.bind(null, trExpr)));
    } else if (expr.type === "ObjectExpression") {
      r.tag = "Expr_object";
      r.fields = toList(expr.properties.map(trProperty));
    } else if (expr.type === "FunctionExpression") {
      checkFuncExpr(expr);
      r.tag = "Expr_function";
      r.func_name_opt = toOption(trIdentifier, expr.id);
      r.arg_names = toList(expr.params.map(trPattern));
      r.body = trBlockStatAsFuncbody(expr.body);
      // TODO: USE STRICT
    } else if (expr.type === "SequenceExpression") {
      r = expr.expressions.map(trExpr).reduce(function (previousValue, currentValue) {
        return {
          type: "expr",
          tag: "Expr_binary_op",
          arg1: previousValue,
          op: { type: "binary_op", tag: "Binary_op_coma" },
          arg2: currentValue,
          loc: {
            file: previousValue.loc.file,
            start: previousValue.loc.start,
            end: currentValue.loc.end
          }
        };
      });
    } else if (expr.type === "UnaryExpression") {
      r.tag = "Expr_unary_op";
      r.op = trUnaryOp(expr.operator);
      // expr.prefix (boolean) unused
      r.arg = trExpr(expr.argument);
    } else if (expr.type === "BinaryExpression") {
      r.tag = "Expr_binary_op";
      r.arg1 = trExpr(expr.left);
      r.op = trBinaryOp(expr.operator);
      r.arg2 = trExpr(expr.right);
    } else if (expr.type === "AssignmentExpression") {
      r.tag = "Expr_assign";
      r.left_expr = trExpr(expr.left);
      r.op_opt = trAssignmentOp(expr.operator);
      r.right_expr = trExpr(expr.right);
    } else if (expr.type === "UpdateExpression") {
      r.tag = "Expr_unary_op";
      r.op = trUpdateOp(expr.operator, expr.prefix);
      r.arg = trExpr(expr.argument);
    } else if (expr.type === "LogicalExpression") {
      r.tag = "Expr_binary_op";
      r.arg1 = trExpr(expr.left);
      r.op = trLogicalOp(expr.operator);
      r.arg2 = trExpr(expr.right);
    } else if (expr.type === "ConditionalExpression") {
      r.tag = "Expr_conditional";
      r.cond = trExpr(expr.test);
      r.then_branch = trExpr(expr.consequent);
      r.else_branch = trExpr(expr.alternate);
    } else if (expr.type === "NewExpression") {
      r.tag = "Expr_new";
      r.func = trExpr(expr.callee);
      r.args = toList(expr.arguments.map(trExpr));
    } else if (expr.type === "CallExpression") {
      r.tag = "Expr_call";
      r.func = trExpr(expr.callee);
      r.args = toList(expr.arguments.map(trExpr));
    } else if (expr.type === "MemberExpression" && !expr.computed) {
      r.tag = "Expr_member";
      r.obj = trExpr(expr.object);
      r.field_name = trIdentifier(expr.property);
    } else if (expr.type === "MemberExpression" && expr.computed) {
      r.tag = "Expr_access";
      r.obj = trExpr(expr.object);
      r.field = trExpr(expr.property);
    } else if (expr.type === "Identifier") {
      r.tag = "Expr_identifier";
      r.name = trIdentifier(expr);
    } else if (expr.type === "Literal") {
      r.tag = "Expr_literal";
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
  unary_operators["-"] = "Unary_op_neg";
  unary_operators["+"] = "Unary_op_add";
  unary_operators["!"] = "Unary_op_not";
  unary_operators["~"] = "Unary_op_bitwise_not";
  unary_operators["void"] = "Unary_op_void";
  unary_operators["typeof"] = "Unary_op_typeof";
  unary_operators["delete"] = "Unary_op_delete";
  var trUnaryOp = function (op) {
    return { type: "unary_op", tag: unary_operators[op] };
  };

  var binaryOpTagToObj = function (tag) {
    return { type: "binary_op", tag: tag };
  };

  const binary_operators = Object.create(null);
  binary_operators["=="] = "Binary_op_equal";
  binary_operators["!="] = "Binary_op_disequal";
  binary_operators["==="] = "Binary_op_strict_equal";
  binary_operators["!=="] = "Binary_op_strict_disequal";
  binary_operators["<"] = "Binary_op_lt";
  binary_operators["<="] = "Binary_op_le";
  binary_operators[">"] = "Binary_op_gt";
  binary_operators[">="] = "Binary_op_ge";
  binary_operators["<<"] = "Binary_op_left_shift";
  binary_operators[">>"] = "Binary_op_right_shift";
  binary_operators[">>>"] = "Binary_op_unsigned_right_shift";
  binary_operators["+"] = "Binary_op_add";
  binary_operators["-"] = "Binary_op_sub";
  binary_operators["*"] = "Binary_op_mult";
  binary_operators["/"] = "Binary_op_div";
  binary_operators["%"] = "Binary_op_mod";
  binary_operators["|"] = "Binary_op_bitwise_or";
  binary_operators["^"] = "Binary_op_bitwise_xor";
  binary_operators["&"] = "Binary_op_bitwise_and";
  binary_operators["in"] = "Binary_op_in";
  binary_operators["instanceof"] = "Binary_op_instanceof";
  var trBinaryOp = function (op) {
    return binaryOpTagToObj(binary_operators[op]);
  };

  const assignment_operators = Object.create(null);
  assignment_operators["="] = null;
  assignment_operators["+="] = "Binary_op_add";
  assignment_operators["-="] = "Binary_op_sub";
  assignment_operators["*="] = "Binary_op_mult";
  assignment_operators["/="] = "Binary_op_div";
  assignment_operators["%="] = "Binary_op_mod";
  assignment_operators["<<="] = "Binary_op_left_shift";
  assignment_operators[">>="] = "Binary_op_right_shift";
  assignment_operators[">>>="] = "Binary_op_unsigned_right_shift";
  assignment_operators["|="] = "Binary_op_bitwise_or";
  assignment_operators["^="] = "Binary_op_bitwise_xor";
  assignment_operators["&="] = "Binary_op_bitwise_and";
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
      tag: "Unary_op_" + fix + "_" + update_operators[op]
    };
  };

  const logical_operators = Object.create(null);
  logical_operators["||"] = "Binary_op_or";
  logical_operators["&&"] = "Binary_op_and";
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
        value = Object.assign({}, value);
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

try {
  // For testing purposes with node
  module.exports.esprimaToAST = esprimaToAST;
  module.exports.EsprimaToASTError = EsprimaToASTError;
  module.exports.UnsupportedSyntaxError = UnsupportedSyntaxError;
  module.exports.toString = toString;
} catch (e) {
  // Ignore these, as we're not using node
}
