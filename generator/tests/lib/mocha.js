/* Mocha-compatible shim for OCaml, JS support implementation, see mocha.mli for documentation */
var Mocha = {};
({ ok   : Mocha.assert_ok
 , fail : Mocha.assert_fail
 , strictEqual: Mocha.assert_bool
 , strictEqual: Mocha.assert_int
 , strictEqual: Mocha.assert_float
 , strictEqual: Mocha.assert_char
 , strictEqual: Mocha.assert_string
 , deepStrictEqual : Mocha.assert_struct_eq
 , throws: Mocha.throws
} = require('assert'));
var old_it = it;
it = function(string, callback) {
  old_it(string, function(done) {
    callback();
    done();
  })
}
Mocha.assert_unit = x => {
  Mocha.assert_struct_eq(x, {}, x + " is not unit ({}).")
}
Mocha.assert_failwith = (f, e, m) => Mocha.throws(f, RegExp(e), m);
