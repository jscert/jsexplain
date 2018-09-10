/* Convert test262's declared set of features into a source file for import */
const fs = require('fs');

const existingFeatures = require('./supported-features.js');
const features = new Set();

const generateSource = (features, existingFeatures) => {
  return [
    '/* List of supported test262 features.',
    ' * This list can be regenerated using the generate-all-features.js script. */',
    'module.exports = new Set([',
    ...Array.from(features).sort().map(entry => `  ${existingFeatures.has(entry) ? '' : '//'}'${entry}',`),
    ']);',
    ''
  ].join('\n');
}

fs.readFile('test/data/test262/features.txt', 'utf8', (err, data) => {
  if (err) throw err;
  for (const line of data.split('\n')) {
    const trim = line.trim();
    if (trim && !trim.startsWith('#')) {
      features.add(trim);
    }
  }

  fs.writeFile('test/helpers/supported-features.js', generateSource(features, existingFeatures), err => {throw err});
});
