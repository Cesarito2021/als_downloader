const assert = require('node:assert/strict'), fs = require('node:fs'), vm = require('node:vm');
const code = require('esbuild').transformSync(fs.readFileSync(__dirname + '/reader.js', 'utf8'), {format: 'cjs'}).code;
const context = {module: {exports: {}}, require, Uint8Array, Number, fetch};
vm.runInNewContext(code, context);
const {rangeGetter} = context.module.exports;
(async () => {
  const signal = new AbortController().signal;
  let canceled = false;
  const ignored = rangeGetter('https://example.org', signal, async () => ({status: 200, body: {cancel: async () => canceled = true}}));
  await assert.rejects(() => ignored(0, 10), /206/); assert(canceled);
  const valid = rangeGetter('https://example.org', signal, async () => new Response(new Uint8Array(10),
    {status: 206, headers: {'Content-Range': 'bytes 0-9/100'}}), 10);
  assert.equal((await valid(0, 10)).length, 10);
  await assert.rejects(() => valid(0, 1), /budget/);
  const malformed = rangeGetter('https://example.org', signal, async () => new Response(new Uint8Array(10),
    {status: 206, headers: {'Content-Range': 'bytes 1-10/100'}}));
  await assert.rejects(() => malformed(0, 10), /Content-Range/);
  const oversize = rangeGetter('https://example.org', signal, async () => new Response(new Uint8Array(11),
    {status: 206, headers: {'Content-Range': 'bytes 0-9/100'}}));
  await assert.rejects(() => oversize(0, 10), /Oversized/);
  const short = rangeGetter('https://example.org', signal, async () => new Response(new Uint8Array(9),
    {status: 206, headers: {'Content-Range': 'bytes 0-9/100'}}));
  await assert.rejects(() => short(0, 10), /Incomplete/);
  console.log('PASS: partial responses, ignored ranges, byte limits, mismatched headers, oversized and incomplete bodies.');
})().catch(e => {console.error(e); process.exitCode = 1;});
