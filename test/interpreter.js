"use strict";

const fs = require('fs');
const assert = require('assert');
// tripwire module is no longer maintained
//const tripwire = require('tripwire');


// Stub logging functions
['ctx_empty', 'ctx_push', 'log_event'].forEach(f => global[f] = function(){})

const esprima = require('esprima');
const esprimaToAST = require('../esprima-to-ast.js').esprimaToAST;
const jsref = require('../jsref/assembly.js');

const timeout = 5000;

require('./parser.js');
var test262 = require('./helpers/test262.js');

var parse = function(source) {
  return esprimaToAST(esprima.parse(source, {loc: true, range: true}), source);
}

var run = function(program) {
  return jsref.JsInterpreter.run_javascript(program);
};

var prelude;
before(function(done) {
  fs.readFile(__dirname + '/data/test_prelude.js', (err, data) => {
    if (err) throw err;
    prelude = jsref.JsInterpreter.run_javascript(parse(data));
    testResultForException(prelude, undefined);
    done();
  });
});

function Test262Error(message) {
  this.name = "Test262Error";
  this.message = "Test triggered prelude $ERROR:\n" + message || "Test262 $ERROR";
}
Test262Error.prototype = Object.create(Error.prototype);
Test262Error.prototype.constructor = Test262Error;

function unpack_value(v) {
  switch (v.tag) {
    case "Value_undef":
      return undefined;
    case "Value_null":
      return null;
    case "Value_bool":
      return (v.value) ? true : false;
    case "Value_number":
      return v.value;
    case "Value_string":
      return v.value;
    default:
      throw "unrecognized tag in unpack_value";
  }
}

function getExceptionFromValue(state, value) {
  let vname, vmessage;
  let name = jsref.JsInterpreterUtils.get_object_value(state, value, "name");
  if (name.tag === 'Some') {
    vname = unpack_value(name.value);
  }
  let message = jsref.JsInterpreterUtils.get_object_value(state, value, "message");
  if (message.tag === 'Some') {
    vmessage = unpack_value(message.value);
  }

  return { name: vname, message: vmessage };
};

function testResultForException(result, negative) {
  if (result.tag !== "Result_some") {
    throw new Error(`Execution resulted in ${result.tag}.`);
  }

  var state = result.value.state;
  var completion = result.value.res;
  var value = completion.res_value.value;

  switch (completion.res_type.tag) {
    case "Restype_throw":
      // We got an exception!
      let exc = getExceptionFromValue(state, value);

      if (!negative) {
        let msg = "Test threw unexpectedly";
        // But we were not expecting one :(

        if (value.tag === "Value_prim") {
          msg += "\nThrown primitive value: " + unpack_value(value.value);
        } else if (value.tag === "Value_object") {
          msg += "\nException type: " + exc.name;
          msg += "\nException message: " + exc.message;
        }
        throw new Error(msg);

      } else if (negative.phase === "runtime") {
        // We were expecting a specific exception
        if (negative.type !== exc.name) {
          throw new Error(`Test should have thrown a ${negative.type} error, but we got a ${exc.name} instead!`);
        }
      } else {
        throw new Error(`Test should have thrown at ${negative.phase} but actually at runtime.`);
      }

      break;

    case "Restype_normal":
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

test262.addTest(getTest => {
  it("interprets correctly?", async function() {
    const test = getTest();
    let initHeap = prelude;

    try {
      var ast = parse(test.contents);
    } catch(e) { this.skip(); }

    for (const include of test.attrs.includes) {
      const content = await test262.fetchHarness(include);
      const contentAst = parse(content);
      initHeap = jsref.JsInterpreter.run_javascript_from_result(initHeap, contentAst);
      assert.doesNotThrow(() => testResultForException(initHeap, undefined),
        `initHeap execution threw on ${include}.`);
    }

    this.timeout(timeout);
    //tripwire.resetTripwire(timeout);

    var result = jsref.JsInterpreter.run_javascript_from_result(initHeap, ast);

    //tripwire.clearTripwire();

    // FIXME: Temporary use only.
    if (result.tag === "Result_some") {
      testResultForException(result, test.attrs.negative);
    } else if (result.tag === "Result_not_yet_implemented") {
      console.log(`Execution resulted in ${result.tag}.`);
      this.skip();
    } else {
      throw Error(`Execution resulted in ${result.tag}.`);
    }
  });
});

