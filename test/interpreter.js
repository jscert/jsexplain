"use strict";

var esprima = require('esprima');
var esprimaToAST = require('../esprima-to-ast.js').esprimaToAST;
var JsInterpreter = require('../generator/tests/jsref/assembly.js');

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

console.dir(parse_and_run_multiple(["var x = 10", "x+1"]), {depth: 6, colors: true});
console.dir(parse_and_run_multiple(["throw 10", "x+1"]), {depth: 6, colors: true});
