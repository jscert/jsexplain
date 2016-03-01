function esprimaToAST(prog) {
  var toList = function (array) {
    var r = {tag: "[]"};
    for (var i = array.length - 1; i >= 0; i--) {
      r = {tag: "::", head: array[i], tail: r};
    }
    return r;
  };

  var toLoc = function (pos) {
    if (pos === null) {throw "null position in esprima AST";};
    return {file: "input.js",
            start: {line: pos.start.line, col: pos.start.column},
            stop:  {line: pos.end.line, col: pos.end.column}};
  };

  var trProg = function (prog) {
    if (prog.type !== "Program") {
      throw ("trProg error: " + prog.type);
    };
    var r = {loc: toLoc(prog.loc), type: "prog"};
    r.tag = "Coq_prog_intro";
    // TODO deal with strictness
    r.strictness = false;
    r.elements = toList(prog.body.map(trStatAsElement));
    return r;
  };

  var trStatAsElement = function (stat) {
    var loc = toLoc(stat.loc);
    var r = {loc: loc, type: "element"};
    if (stat.type === "FunctionDeclaration") {
      r.tag = "Coq_element_func_decl";
      r.func_name = stat.id ;
      r.arg_names = trParams(r.params);
      // TODO this could be detected by stat.expression of type bool
      if (stat.body.type !== "BlockStatement") {
        throw ("stat.body.type !== BlockStatement");
      };
      // TODO we don't know how to get source
      var blockStat = stat.body;
      var prog = {loc: loc, type: "prog", tag: "Coq_prog_intro",
                  strictness: false,
                  elements: toList(blockStat.body.map(trStatAsElement))};
      r.body = {loc: loc, type: "funcbody", tag: "Coq_funcbody_intro",
                prog: prog, source: ""};
    } else /* if (stat.type <: "Statement") */  {
      r.tag = "Coq_element_stat";
      r.stat = trStat(stat);
    } // else {
    //   throw ("trStatAsElement error: " + stat.type);
    // }
    return r;
  };

  // return string
  var trPattern = function (pat) {
    if (pat.type === "Identifier") {
      return pat.name;
    } else {
      throw ("trPattern error: " + pat.type);
    }
  };

  // return the encoding of a pair
  var trVarDecl = function (decl) {
    var id = trPattern(decl.id);
    var eo = {type: "option"};
    if (decl.init === null) {
      eo.tag = "None";
    } else {
      eo.tag = "Some";
      eo.value = trExpr(decl.init);
    }
    return [id,eo];
  };

  var trStat = function (stat) {
    var r = {loc: toLoc(stat.loc), type: "stat"};
    if (stat.type === "ExpressionStatement") {
      r.tag = "Coq_stat_expr";
      // TODO why is it different than Parser API that says expr
      r.expr = trExpr(stat.expression);
    } else if (stat.type === "BlockStatement") {
      r.tag = "Coq_stat_block";
      r.stats = toList(stat.body.map(trStat));
    } else if (stat.type === "VariableDeclaration") {
      r.tag = "Coq_stat_var_decl";
      r.decls = toList(stat.declarations.map(trVarDecl));
    } else {
      throw ("trStat error: " + stat.type);
    };
    return r;
  };

  var trExprAsLiteral = function (expr) {
    var r = {loc: toLoc(expr.loc), type: "literal"};
    var v = expr.value;
    var t = typeof(v);
    if (t === "null") {
      r.tag = "Coq_literal_null";
    } else if (t === "bool") {
      r.tag = "Coq_literal_bool";
      r.value = v;
    } else if (t === "number") {
      r.tag = "Coq_literal_number";
      r.value = v;
    } else if (t === "string") {
      r.tag = "Coq_literal_string";
      r.value = v;
    } else {
      throw ("trExprAsLiteral error: " + t);
    };
    return r;
  };

  var trExpr = function (expr) {
    var r = {loc: toLoc(expr.loc), type: "expr"};
    if (expr.type === "ThisExpression") {
      r.tag = "Coq_expr_this";
    } else if (expr.type === "Identifier") {
      r.tag = "Coq_expr_identifier";
      r.name = expr.name;
    } else if (expr.type === "Literal") {
      r.tag = "Coq_expr_literal";
      r.value = trExprAsLiteral(expr);
    } else {
      throw ("trExpr error: " + expr.type);
    };
    return r;
  };

  return trProg(prog);
}

function testParse(s) {
  var p = esprima.parse(s,{loc: true});
  console.log(p);
  console.log(esprimaToAST(p));
}
