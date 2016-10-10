"use strict";

var esprima = require('esprima');
var esprimaToAST = require('../esprima-to-ast.js').esprimaToAST;
var JsInterpreter = require('../jsref/assembly.js');

require('./parser.js');
var test262tests = require('./helpers/test262.js');

// Stub logging functions
['ctx_empty', 'ctx_push', 'log_event'].forEach(f => global[f] = function(){})

var parse = function(source) {
  return esprimaToAST(esprima.parse(source, {loc: true, range: true}), source);
}

var run = function(program) {
  return JsInterpreter.run_javascript(program);
};

// Functions to run multiple programs in the same heap
// Useful for correctly loading the test262 prelude before the testcase
var run_multiple = function(programs) {
  if(programs.length < 1) { throw "No programs" }
  var firstResult = run(programs.shift());
  return programs.reduce(JsInterpreter.run_javascript_from_result, firstResult);
};

var parse_and_run_multiple = function(sources) {
  var programs = sources.map(parse);
  return run_multiple(programs);
}

Error.stackTraceLimit = 30;

test262tests.push(args => {
  it("interprets", function() {
    try {
      var ast = parse(args.source);
    } catch(e) { return; }

    var result = JsInterpreter.run_javascript(ast);
  });
});

test262tests.push(args => {
  it.skip("executes correctly");
  // TODO
  // This test should be combined with the interprets test
  // It also needs to:
  // * Load the test262 prelude functions
  // * Execute the test in this heap
  // * Check the result of the testcase
});
