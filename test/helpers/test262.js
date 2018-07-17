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
const path = require('path');
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
  if (!process.env.CI || process.env.CI === 'false') {
    oldReporterList(failures);
  } else {
    console.log(Base.color('fail', 'Failure stack traces are suppressed in the CI environment.'));
  }
};

const testDataDir = path.join(__dirname, '..', 'data');
const test262path = fs.readlinkSync(path.join(testDataDir, 'test262'));

// Run the given callback at the end of this I/O Loop, so that testConstructors may be registered before constructing
// test cases.
setImmediate(() => {
  const tests = [];

  walk(path.join(test262path, 'test'))
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

                // Skip the test if it uses features that we don't support.
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

const harnessCache = new Map();
function fetchHarness(name) {
  if (harnessCache.has(name)) {
    return harnessCache.get(name);
  }
  const content = fs.readFileSync(path.join(test262path, 'harness', name));
  harnessCache.set(name, content);
  return content;
}

module.exports = {
  addTest: test => testConstructors.push(test),
  fetchHarness: fetchHarness
}
