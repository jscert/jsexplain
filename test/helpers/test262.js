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

const testConstructors = [];

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
              .then(data => { test = test262parser.parseFile({ file: path, contents: data.toString() }); })
              .then(doneFile);
          });

          testConstructors.forEach(constructor => constructor(() => test));
        });
      });
    });

    run();
  });
});

module.exports = testConstructors;
