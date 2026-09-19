import { preview } from './reader.js';
const status = document.getElementById('status');
let controller;
document.getElementById('cancel').onclick = () => controller?.abort();
document.getElementById('load').onclick = async () => {
  controller?.abort();
  const active = new AbortController(); controller = active;
  const timer = setTimeout(() => active.abort(), 60000);
  status.textContent = 'Reading partial COPC ranges…';
  try {
    const result = await preview(document.getElementById('url').value.trim(), active.signal);
    if (active !== controller) return;
    window.handlers['als-points']({...result, target: 'als-cloud'});
    status.textContent = `${result.points.length.toLocaleString()} displayed points; ${(result.bytes / 1048576).toFixed(2)} MiB requested in ${result.requests} ranges. Coarse display sample, not analysis data.`;
    window.lastPreview = {points: result.points.length, bytes: result.bytes, requests: result.requests};
  } catch (e) {
    if (active === controller) status.textContent = active.signal.aborted ? 'Canceled or timed out.' :
      String(e.message).replace(/https?:\/\/[^\s]+/g, '[source URL]');
  } finally {clearTimeout(timer);}
};
