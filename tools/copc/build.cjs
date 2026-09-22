const fs = require('fs'), path = require('path');
require('esbuild').buildSync({entryPoints: [path.join(__dirname, 'browser.js')], bundle: true,
  platform: 'browser', outfile: path.join(__dirname, 'viewer.js'), legalComments: 'eof'});
const laz = path.dirname(require.resolve('laz-perf/package.json', {paths: [path.dirname(require.resolve('copc'))]}));
fs.copyFileSync(path.join(laz, 'lib/web/laz-perf.wasm'), path.join(__dirname, 'laz-perf.wasm'));
