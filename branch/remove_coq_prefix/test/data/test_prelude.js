/* Test262 Host-Defined Functions
 *
 * These functions must be included in addition to test262/harness/assert.js and test262/harness/sta.js
 * and/or any other files specified by test262/INTERPRETING.md
 *
 * The function documentation for this file is taken directly from test262/INTERPRETING.md
 */

/*
print
  A function that exposes the string value of its first argument to
  the test runner. This is used as a communication mechanism for asynchronous
  tests (via the `async` flag, described below).
*/
var print = function(message) {}

var $262 = {
  /* createRealm
    a function which creates a new [ECMAScript
    Realm](https://tc39.github.io/ecma262/2016/#sec-code-realms),
    defines this API on the new realm's global object, and returns the `$`
    property of the new realm's global object */
  createRealm: function() {},

  /* detachArrayBuffer
   * a function which implements [the
    DetachArrayBuffer abstract
    operation](https://tc39.github.io/ecma262/2016/#sec-detacharraybuffer) */
  detachArrayBuffer: function() {},

  /* evalScript
    a function which accepts a string value as its first
    argument and executes is as [an ECMAScript
    script](https://tc39.github.io/ecma262/2016/#sec-scripts) according to the
    following algorithm:

        1. Let hostDefined be any host-defined values for the provided
           sourceText (obtained in an implementation dependent manner)
        2. Let realm be the current Realm Record.
        3. Let s be ParseScript(sourceText, realm, hostDefined).
        4. If s is a List of errors, then
           a. Let error be the first element of s.
           b. Return
              Completion{[[Type]]: throw, [[Value]]: error, [[Target]]: empty}.
        5. Let status be ScriptEvaluation(s).
        6. Return Completion(status).
    */
  evalScript: function() {},

  /* global
     a reference to the global object on which `$262` was initially defined */
  global: this,


  agent: {
    /* start
      a function that takes a script source string and runs
      the script in a concurrent agent.  Will block until that agent is
      running.  The agent has no representation.  The agent script will be
      run in an environment that has an object `$` with a property `agent`
      with the following properties:
      - receiveBroadcast - a function that takes a function and
        calls the function when it has received a broadcast from the parent,
        passing it the broadcast as two arguments, a SharedArrayBuffer and
        an Int32.  This function may return before a broadcast is received
        (eg to return to an event loop to await a message) and no code should
        follow the call to this function.
      - report - a function that takes a string and places it in a
        transmit queue whence the parent will retrieve it.  Messages
        should be short.
      - sleep - a function that takes a millisecond argument and
        sleeps the agent for approximately that duration.
      - leaving - a function that signals that the agent is done and
        may be terminated (if possible). */
    start: function() {},

    /* broadcast
        a function that takes a SharedArrayBuffer and an Int32
        and broadcasts the two values to all concurrent agents.  The function
        blocks until all agents have retrieved the message.  Note, this assumes
        that all agents that were started are still running. */
    broadcast: function() {},

    /* getReport
      a function that reads an incoming string from any agent,
      and returns it if it exists, or returns `null` otherwise. */
    getReport: function() {},

    /* sleep
       a function that takes a millisecond argument and
       sleeps the execution for approximately that duration. */
    sleep: function() {}
  }
};
// Copyright (c) 2012 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Provides both:

    - An error class to avoid false positives when testing for thrown exceptions
    - A function to explicitly throw an exception using the Test262Error class
---*/


function Test262Error(message) {
  this.message = message || "";
}

Test262Error.prototype.toString = function () {
  return "Test262Error: " + this.message;
};

var $ERROR;
$ERROR = function $ERROR(message) {
  throw new Test262Error(message);
};
// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of assertion functions used throughout test262
---*/

function assert(mustBeTrue, message) {
  if (mustBeTrue === true) {
    return;
  }

  if (message === undefined) {
    message = 'Expected true but got ' + String(mustBeTrue);
  }
  $ERROR(message);
}

assert._isSameValue = function (a, b) {
  if (a === b) {
    // Handle +/-0 vs. -/+0
    return a !== 0 || 1 / a === 1 / b;
  }

  // Handle NaN vs. NaN
  return a !== a && b !== b;
};

assert.sameValue = function (actual, expected, message) {
  if (assert._isSameValue(actual, expected)) {
    return;
  }

  if (message === undefined) {
    message = '';
  } else {
    message += ' ';
  }

  message += 'Expected SameValue(«' + String(actual) + '», «' + String(expected) + '») to be true';

  $ERROR(message);
};

assert.notSameValue = function (actual, unexpected, message) {
  if (!assert._isSameValue(actual, unexpected)) {
    return;
  }

  if (message === undefined) {
    message = '';
  } else {
    message += ' ';
  }

  message += 'Expected SameValue(«' + String(actual) + '», «' + String(unexpected) + '») to be false';

  $ERROR(message);
};

assert.throws = function (expectedErrorConstructor, func, message) {
  if (typeof func !== "function") {
    $ERROR('assert.throws requires two arguments: the error constructor ' +
      'and a function to run');
    return;
  }
  if (message === undefined) {
    message = '';
  } else {
    message += ' ';
  }

  try {
    func();
  } catch (thrown) {
    if (typeof thrown !== 'object' || thrown === null) {
      message += 'Thrown value was not an object!';
      $ERROR(message);
    } else if (thrown.constructor !== expectedErrorConstructor) {
      message += 'Expected a ' + expectedErrorConstructor.name + ' but got a ' + thrown.constructor.name;
      $ERROR(message);
    }
    return;
  }

  message += 'Expected a ' + expectedErrorConstructor.name + ' to be thrown but no exception was thrown at all';
  $ERROR(message);
};

/* FIXME: This breaks
assert.throws.early = function(err, code) {
  var wrappedCode = 'function wrapperFn() { ' + code + ' }';
  var ieval = eval;

  assert.throws(err, function() { Function(wrappedCode); }, 'Function: ' + code);
};
*/
