var fs = require('mz/fs');
var walk = require('klaw');
var filter = require('through2-filter');
fs.readlinkSync = require('readlink').sync; // a non-broken readlink...

var esprima = require('esprima');
var esprimaToAST = require('../esprima-to-ast.js');

var test262path = fs.readlinkSync(__dirname + '/test262');
var tests = [];

/* Tests whether a given test is negative.
 * Returns: a string if type of failure specified, true, or false
 */
function testNegativity(str) {
  var result = /@negative[ \t]*(\S*)?[ \t]*$/m.exec(str);
  if(result) {
    result = result[1] || true;
  } else {
    result = false;
  }
  return result;
}

function isParserNegativeTest(str) {
  var negative = testNegativity(str);
  if (typeof negative === 'boolean') {
    return negative;
  }
  // Second case testing for an Early (Syntax) Error
  return /(?:SyntaxError|\?!NotEarlyError)/.test(negative);
}

walk(test262path)
.pipe(filter.obj(file => file.stats.isFile() && file.path.endsWith(".js")))
.on('readable', function() {
  var item;
  while((item = this.read())) { tests.push(item.path); }
})
.on('end', function() {
  describe("test262", function() {
    tests.forEach(item => {
      describe(item, function() {

        var source;
        var negative = '';

        before(function(done) {
          fs.readFile(item).then(
            data => {
              source = data.toString();
              negative = isParserNegativeTest(source);
            }
          ).then(done);
        });

        it('parses', function() {
          try {
            esprima.parse(source, {loc: true});
          } catch(e) {
            if (!negative) {
              throw e;
            }
          }
        });

        it('converts', function() {
          var prog;
          try {
            prog = esprima.parse(source, {loc: true});
          } catch(e) { return; }

          try {
            esprimaToAST.esprimaToAST(prog);
          } catch (e) {
            if (e instanceof esprimaToAST.UnsupportedSyntaxError) {
            } else {
              throw e;
            }
          }
        });
      })
    });
  });

  run();
});
