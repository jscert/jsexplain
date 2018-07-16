"use strict";

// Registers test cases to be run against Test262
// To add a testcase to be tested against test262:
//   var test262tests = require('./helper-test262.js')'
//   test262tests.push(function testConstructorFunction(getFile) {});
//
// Tests are run in the order added to the array. If inter-module test ordering is required,
// ensure the later test module depends on the earlier one.
//
// The testConstructorFunction should call its first argument to get the test case.
// The test case is an object as returned from the test262-parser library, it will contain
// the fields file, contents, attrs, and async at minimum.

const fs = require('mz/fs');
const walk = require('klaw');
const filter = require('through2-filter');
fs.readlinkSync = require('readlink').sync; // a non-broken readlink...
const test262parser = require('test262-parser');
const supportedFeatures = require('./supported-features.js');

const testConstructors = [];

// Shorten failure output
const Base = require('mocha').reporters.Base;
const oldReporterList = Base.list;
Base.list = failures => {
  const num = process.env.ERROR_NUM ? Number(process.env.ERROR_NUM) : 1;
  if (failures.length > num) {
    oldReporterList(failures.slice(0, num));
    console.log(Base.color('fail', 'Error printout limited to %d failures. Set ERROR_NUM in environment to show more.'), num);
  } else {
    oldReporterList(failures);
  }
};

// Run the given callback at the end of this I/O Loop, so that testConstructors may be registered before constructing
// test cases.
setImmediate(() => {
  const testDataDir = __dirname + '/../data';
  const test262path = fs.readlinkSync(testDataDir + '/test262/test');
  const tests = [];

  walk(test262path)
  .pipe(filter.obj(file => file.stats.isFile() && file.path.endsWith(".js")))
  .on('data', (item) => { tests.push(item.path); })
  .on('end', function() {
    describe("test262", function() {
      if (tests.length === 0) { throw new Error("Unable to find any test262 tests (uninitialised git submodule?)") }
      tests.forEach(path => {
        describe(path, function() {
          // Variable to be lazy-loaded, captured by closure below.
          let test = null;

          // Lazy-load the source file prior to tests
          before(function(doneFile) {
            fs.readFile(path)
              .then(data => {
                test = test262parser.parseFile({ file: path, contents: data.toString() });

                const usedFeatures = new Set(test.attrs.features);
                for (const feature of supportedFeatures) {
                  usedFeatures.delete(feature);
                }
                if (usedFeatures.size > 0) {
                  console.log(Base.color('pending', 'Test skipped due to missing features: %s'), Array.from(usedFeatures).join(', '));
                  this.skip();
                }
              })
              .then(doneFile)
              .catch(doneFile);
          });

          testConstructors.forEach(constructor => constructor(() => test));
        });
      });
    });

    run();
  });
});

module.exports = testConstructors;
