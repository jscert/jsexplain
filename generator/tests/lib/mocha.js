/* Mocha-compatible shim for OCaml, JS support implementation, see mocha.mli for documentation */
var Mocha = {};
({ ok   : Mocha.assert_ok
 , fail : Mocha.assert_fail
 , equal: Mocha.assert_bool
 , equal: Mocha.assert_int
 , equal: Mocha.assert_float
 , equal: Mocha.assert_char
 , equal: Mocha.assert_string
 , deepEqual : Mocha.assert_struct_eq
 , throws: Mocha.throws
} = require('assert').strict);
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
Mocha.assert_failwith = (f, e, m) => Mocha.throws(f, Error(e), m);
