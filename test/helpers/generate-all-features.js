/* Generate a list of all features present in test262 file headers. */
const t262stream = require('test262-stream');

const features = new Set();

const stream = new t262stream('test/data/test262', {
  omitRuntime: true,
});
stream.on('data', test => {
  if (test.attrs.features) {
    for (const feature of test.attrs.features) {
      features.add(feature);
    }
  }
});

stream.on('end', () => {
  console.log('module.exports = new Set([');
  for (const entry of Array.from(features).sort()) {
    console.log(`  //'${entry}',`);
  }
  console.log(']);');
});
