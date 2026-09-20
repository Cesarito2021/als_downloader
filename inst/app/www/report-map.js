/* Centred report composition: original study geometry, selected tiles and basemap. */
(function () {
  let busy = false, bypass = null;
  const ids = ['download_report_pdf', 'download_report_map'];
  const status = text => { const el = document.getElementById('report_map_status'); if (el) el.textContent = text; };
  function finish(message) { busy = false; status(message); }
  document.addEventListener('click', event => {
    const button = event.target.closest('a');
    if (!button || !ids.includes(button.id)) return;
    if (bypass === button.id) { bypass = null; return; }
    if (button.id !== 'download_report_map' && !document.getElementById('report_rgb')?.checked) return;
    event.preventDefault(); event.stopImmediatePropagation();
    if (busy) return;
    busy = true; status('Preparing a centred map...');
    Shiny.setInputValue('report_map_request', {action: button.id, nonce: Date.now()}, {priority: 'event'});
  }, true);
  async function capture(payload) {
    let map, container;
    try {
      container = document.createElement('div');
      container.style.cssText = 'position:fixed;left:5vw;top:8vh;width:90vw;height:80vh;max-width:1200px;max-height:760px;background:white;z-index:10000;box-shadow:0 0 0 100vmax rgba(0,0,0,.65);';
      container.id = 'als-report-capture'; document.body.appendChild(container);
      map = L.map(container, {zoomControl:false, attributionControl:true, zoomAnimation:false, fadeAnimation:false,
        preferCanvas:true,renderer:L.canvas({padding:0})});
      const region = L.geoJSON(payload.aoi, {style:{color:'#ffe4a3',weight:4,fill:false,dashArray:'9,5'}});
      const footprints = L.geoJSON(payload.tiles, {style:{color:'#18212a',weight:2.5,fillColor:'#18212a',fillOpacity:0.15}}).addTo(map);
      region.addTo(map); region.bringToFront();
      // Include full selected tile edges as well as the actual, possibly irregular AOI.
      map.fitBounds(region.getBounds().extend(footprints.getBounds()), {padding:[60,60],maxZoom:18,animate:false});
      L.control.scale({imperial:false,position:'bottomleft',maxWidth:180}).addTo(map);
      const legend = L.control({position:'topright'});
      legend.onAdd = () => {
        const el = L.DomUtil.create('div');
        el.style.cssText='background:rgba(255,255,255,.94);padding:12px 16px;color:#18212a;font:16px Arial;line-height:1.6';
        el.innerHTML='<b>AOI and selected LiDAR tiles</b><br><span style="border-top:4px dashed #b58927;display:inline-block;width:30px"></span> AOI<br><span style="border-top:3px solid #18212a;display:inline-block;width:30px"></span> Selected tile footprints';
        return el;
      }; legend.addTo(map);
      const imagery = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        crossOrigin:'anonymous',maxZoom:19,keepBuffer:0,
        attribution:'&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      });
      await new Promise((resolve,reject) => {
        const timer=setTimeout(()=>reject(Error('Map tiles timed out. Retry, or turn off the basemap option to use the footprint map.')),25000);
        imagery.once('load',()=>{clearTimeout(timer);resolve();});
        imagery.once('tileerror',()=>{clearTimeout(timer);reject(Error('Map tiles could not be loaded. Retry, or turn off the basemap option.'));});
        imagery.addTo(map);
      });
      const images=[...container.querySelectorAll('img.leaflet-tile')];
      if (!images.length || images.some(img=>!img.complete || !img.naturalWidth)) throw Error('Map tiles are incomplete; no report map was saved.');
      const copies=new Map();
      for (const img of images) {
        const tile=document.createElement('canvas');tile.width=img.naturalWidth;tile.height=img.naturalHeight;
        tile.getContext('2d').drawImage(img,0,0);copies.set(img.src,tile.toDataURL('image/png'));
      }
      const credits=(container.querySelector('.leaflet-control-attribution')?.textContent || imagery.options.attribution)
        .replace(/\u00a9/g,'(c)').replace(/[\u2013\u2014]/g,'-') + ' | https://www.openstreetmap.org/copyright';
      const canvas=await html2canvas(container,{scale:1.5,logging:false,useCORS:true,backgroundColor:'#ffffff',
        onclone:doc=>doc.getElementById(container.id).querySelectorAll('img.leaflet-tile').forEach(img=>{img.src=copies.get(img.src);})});
      const out=document.createElement('canvas'),ctx=out.getContext('2d');ctx.font='18px Arial';
      const lines=[];
      for(const note of ['ALS Downloader | Basemap: (c) OpenStreetMap contributors, https://www.openstreetmap.org/copyright | Cartographic context, not LiDAR acquisition conditions.',...(payload.sources || [])]) {
        let line='';
        for(const ch of String(note)) {
          if(ctx.measureText(line+ch).width>canvas.width-40){lines.push(line);line='';}
          line+=ch;
        }
        if(line)lines.push(line);
      }
      if(lines.length>100)throw Error('Too many source credits for one map image. Select a smaller set of datasets.');
      out.width=canvas.width;out.height=canvas.height+28+lines.length*24;
      ctx.fillStyle='#fff';ctx.fillRect(0,0,out.width,out.height);ctx.drawImage(canvas,0,0);
      ctx.font='18px Arial';ctx.fillStyle='#18212a';lines.forEach((line,i)=>ctx.fillText(line,20,canvas.height+24+i*24));
      Shiny.setInputValue('report_map_result',{token:payload.token,png:out.toDataURL('image/png'),credits},{priority:'event'});
      status('basemap map ready. Preparing your download...');
    } catch(e) {
      Shiny.setInputValue('report_map_result',{token:payload.token,error:e.message},{priority:'event'});
      finish(e.message);
    } finally { if(map)map.remove(); if(container)container.remove(); }
  }
  $(document).on('shiny:connected',()=>{
    Shiny.addCustomMessageHandler('als-report-map',capture);
    Shiny.addCustomMessageHandler('als-report-map-error',message=>finish(message));
    Shiny.addCustomMessageHandler('als-report-map-ready',id=>{
      finish('Centred map ready.');bypass=id;document.getElementById(id)?.click();
    });
  });
  $(document).on('shiny:disconnected',()=>finish('Connection closed. Reconnect before creating a report.'));
})();
