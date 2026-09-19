import { Copc, Las } from 'copc';

// A bounded display prototype. No whole-file fallback, retries or analysis outputs.
export function rangeGetter(url, signal, fetcher = fetch, budget = 16 * 1024 * 1024) {
  let bytes = 0, requests = 0;
  const get = async (begin, end) => {
    const length = end - begin;
    if (!Number.isSafeInteger(begin) || !Number.isSafeInteger(end) || begin < 0 ||
        length <= 0 || length > 4 * 1024 * 1024 || bytes + length > budget || requests >= 128)
      throw new Error('Preview transfer budget exceeded.');
    bytes += length; requests++;
    const response = await fetcher(url, {headers: {Range: `bytes=${begin}-${end - 1}`}, signal,
      credentials: 'omit', referrerPolicy: 'no-referrer'});
    if (response.status !== 206) {
      await response.body?.cancel();
      throw new Error('This source did not honor a partial request (HTTP 206 required).');
    }
    const range = response.headers.get('content-range');
    const match = range?.match(/^bytes (\d+)-(\d+)\/(\d+|\*)$/);
    // Some S3 buckets permit ranges but do not expose Content-Range to JS.
    // Still require 206 and an exactly bounded body; validate offsets when visible.
    if (range !== null && (!match || Number(match[1]) !== begin || Number(match[2]) !== end - 1)) {
      await response.body?.cancel();
      throw new Error('Source returned a mismatched Content-Range header.');
    }
    const reader = response.body.getReader(), parts = [];
    let size = 0;
    try {
      while (true) {
        const {done, value} = await reader.read();
        if (done) break;
        size += value.length;
        if (size > length) throw new Error('Oversized partial response.');
        parts.push(value);
      }
    } catch (e) { await reader.cancel(); throw e; }
    if (size !== length) throw new Error('Incomplete partial response.');
    const out = new Uint8Array(size); let offset = 0;
    for (const part of parts) { out.set(part, offset); offset += part.length; }
    return out;
  };
  get.stats = () => ({bytes, requests});
  return get;
}

export async function preview(url, signal) {
  if (new URL(url).protocol !== 'https:') throw new Error('Use an HTTPS COPC URL.');
  const get = rangeGetter(url, signal);
  const copc = await Copc.create(get);
  const subtree = await Copc.loadHierarchyPage(get, copc.info.rootHierarchyPage);
  // Breadth-first nodes from the root page give a coarse, spatially distributed
  // view. This deliberately does not pretend to be camera-adaptive streaming.
  const nodes = Object.entries(subtree.nodes).sort(([a], [b]) =>
    Number(a.split('-')[0]) - Number(b.split('-')[0]));
  const lazPerf = await Las.PointData.createLazPerf({locateFile: () => 'laz-perf.wasm'});
  const origin = copc.header.min, points = [];
  let decoded = 0;
  for (const [, node] of nodes) {
    if (signal.aborted) throw new Error('Preview canceled.');
    if (node.pointCount <= 0 || node.pointCount > 750000 ||
        decoded + node.pointCount > 750000 || node.pointDataLength > 4 * 1024 * 1024) continue;
    if (points.length >= 100000) break;
    const view = await Copc.loadPointDataView(get, copc, node, {lazPerf, include: ['X', 'Y', 'Z']});
    decoded += node.pointCount;
    const xyz = ['X', 'Y', 'Z'].map(view.getter);
    const every = Math.max(1, Math.ceil(view.pointCount / (100000 - points.length)));
    for (let i = 0; i < view.pointCount; i += every) {
      const p = xyz.map((f, j) => f(i) - origin[j]);
      if (p.every(Number.isFinite)) points.push(p);
    }
  }
  if (!points.length) throw new Error('No root-page nodes fit the preview budget.');
  return {points, origin, ...get.stats(), decoded};
}
