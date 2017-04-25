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
