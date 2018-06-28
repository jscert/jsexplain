"use strict";

const fs = require('fs');
const assert = require('assert').strict;
// tripwire module is no longer maintained
//const tripwire = require('tripwire');


// Stub logging functions
['ctx_empty', 'ctx_push', 'log_event'].forEach(f => global[f] = function(){})

const esprima = require('esprima');
const esprimaToAST = require('../esprima-to-ast.js').esprimaToAST;
const jsref = require('../jsref/assembly.js');

const timeout = 5000;

require('./parser.js');
var test262tests = require('./helpers/test262.js');

var parse = function(source) {
  return esprimaToAST(esprima.parse(source, {loc: true, range: true}), source);
}

var run = function(program) {
  return jsref.JsInterpreter.run_javascript(program);
};

Error.stackTraceLimit = 30;

var prelude;
before(function(done) {
  fs.readFile(__dirname + '/data/test_prelude.js', (err, data) => {
    if (err) throw err;
    prelude = jsref.JsInterpreter.run_javascript(parse(data));
    assert.doesNotThrow(() => testResultForException(prelude, false), "Prelude execution threw!");
    done();
  });
});

function Test262Error(message) {
  this.name = "Test262Error";
  this.message = "Test triggered prelude $ERROR:\n" + message || "Test262 $ERROR";
}
Test262Error.prototype = Object.create(Error.prototype);
Test262Error.prototype.constructor = Test262Error;

function testPreludeError(result) {
  // FIXME: Reintrouduce this error case to prelude
  var state = result.value.state;

  var error = jsref.JsInterpreterUtils.get_global_value(state, "__$ERROR__");

  if (error.tag === 'Some') {
    throw new Test262Error(error.value.value.value);
  }
};

function jsvalue_of_prim(v) {
  switch (v.tag) {
  case "Coq_prim_undef":
    return undefined;
  case "Coq_prim_null":
    return null;
  case "Coq_prim_bool":
    return (v.value) ? true : false;
  case "Coq_prim_number":
    return v.value;
  case "Coq_prim_string":
    return v.value;
  default:
    throw "unrecognized tag in jsvalue_of_prim";
  }
}

function getExceptionFromValue(state, value) {
  let vname, vmessage;
  let name = jsref.JsInterpreterUtils.get_object_value(state, value, "name");
  if (name.tag === 'Some' && name.value.tag === 'Coq_value_prim') {
    vname = jsvalue_of_prim(name.value.value);
  }
  let message = jsref.JsInterpreterUtils.get_object_value(state, value, "message");
  if (message.tag === 'Some' && message.value.tag === 'Coq_value_prim') {
    vmessage = jsvalue_of_prim(message.value.value);
  }

  return { name: vname, message: vmessage };
};

function testResultForException(result, negative) {
  var state = result.value.state;
  var completion = result.value.res;
  var value = completion.res_value.value;

  switch (completion.res_type.tag) {
    case "Coq_restype_throw":
      // We got an exception!
      let exc = getExceptionFromValue(state, value);

      if (!negative) {
        let msg = "Test threw unexpectedly";
        // But we were not expecting one :(

        if (value.tag === "Coq_value_prim") {
          msg += "\nThrown primitive value: " + jsvalue_of_prim(value.value);
        } else if (value.tag === "Coq_value_object") {
          msg += "\nException type: " + exc.name
          msg += "\nException message: " + exc.message;
        }
        throw new Error(msg);

      } else if (typeof negative === "string") {
        // We were expecting a specific exception
        let pat = new RegExp(negative);
        if (!pat.test(exc.name)) {
          throw new Error("Test should have thrown a "+negative+" error, but we got a "+exc.name+" instead!");
        }
      } else {
        // We expected it :)
      }

      break;

    case "Coq_restype_normal":
      if (negative) {
        let msg = "Expected test to throw, but it didn't.";
        if (typeof negative === "string") {
          msg += " Expected exception type: " + negative;
        }
        throw new Error(msg);
      }
      break;

    default:
      throw new Error("Non-normal or throw result type.");
  }
};

test262tests.push(args => {
  it("interprets correctly?", function() {
    try {
      var ast = parse(args.source);
    } catch(e) { this.skip(); }

    this.timeout(timeout);
    //tripwire.resetTripwire(timeout);

    var result = jsref.JsInterpreter.run_javascript_from_result(prelude, ast);

    //tripwire.clearTripwire();

    testPreludeError(result);
    testResultForException(result, args.negative);
  });
});

