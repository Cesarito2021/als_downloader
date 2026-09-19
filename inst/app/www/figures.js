/* Local-only PNG export. No screenshot is uploaded to a service. */
(function () {
  function wrap(ctx,text,width){
    const lines=[];let line='';
    for(const word of String(text).split(/\s+/)){
      if(ctx.measureText(line+' '+word).width>width&&line){lines.push(line);line='';}
      // Long filenames/URLs also wrap without being cut off.
      for(const ch of (line?' ':'')+word){
        if(ctx.measureText(line+ch).width>width&&line){lines.push(line);line='';}
        line+=ch;
      }
    }
    if(line)lines.push(line);return lines;
  }
  async function save(source,filename,notes=[],status,keys=[]){
    try{
      if(!source?.width||!source?.height)throw Error('Load a figure first.');
      const width=Math.min(2400,Math.max(1000,source.width));
      const height=Math.round(source.height/source.width*width);
      const out=document.createElement('canvas'),g=out.getContext('2d');
      g.font='16px sans-serif';
      const lines=notes.flatMap(text=>wrap(g,text,width-48));
      const columns=Math.max(1,Math.floor((width-48)/330));
      const keyHeight=keys.length?Math.ceil(keys.length/columns)*28+16:0;
      out.width=width;out.height=height+keyHeight+32+lines.length*24;
      g.fillStyle='#101b24';g.fillRect(0,0,out.width,out.height);
      g.drawImage(source,0,0,width,height);g.font='16px sans-serif';g.fillStyle='#d8e2e9';
      keys.forEach((key,i)=>{
        const x=24+(i%columns)*330,y=height+24+Math.floor(i/columns)*28;
        g.fillStyle=key.color;g.fillRect(x,y-12,12,12);g.fillStyle='#d8e2e9';
        g.fillText(key.label,x+20,y,300);
      });
      lines.forEach((line,i)=>g.fillText(line,24,height+keyHeight+28+i*24));
      const blob=await new Promise(resolve=>out.toBlob(resolve,'image/png'));
      if(!blob)throw Error('Could not encode the image.');
      const url=URL.createObjectURL(blob),a=document.createElement('a');
      a.href=url;a.download=filename;a.click();setTimeout(()=>URL.revokeObjectURL(url),10000);
      if(status)status.textContent='PNG ready.';
    }catch(e){if(status)status.textContent=e.message;else alert(e.message);}
  }
  window.ALSFigures={save};
  document.addEventListener('DOMContentLoaded',()=>{
    const button=document.getElementById('export_map_png');if(!button)return;
    button.onclick=async()=>{
      const status=document.getElementById('map_export_status'),map=document.getElementById('map');
      button.disabled=true;status.textContent='Preparing map image...';
      try{
        const rect=map.getBoundingClientRect();
        if(!rect.width||!rect.height)throw Error('Open the map before exporting.');
        const include=document.getElementById('map_export_basemap').checked;
        const tiles=[...map.querySelectorAll('img.leaflet-tile')].filter(img=>{
          const r=img.getBoundingClientRect();return r.width&&r.height&&r.right>rect.left&&r.left<rect.right&&r.bottom>rect.top&&r.top<rect.bottom;
        });
        const copies=new Map();
        if(include){
          await Promise.all(tiles.map(async img=>{
            if(!img.complete||!img.naturalWidth)throw Error('Map tiles are still loading. Try again after the map finishes.');
            const safe=new Image();safe.crossOrigin='anonymous';
            await new Promise((resolve,reject)=>{
              const timer=setTimeout(()=>reject(Error('Basemap capture timed out. Retry or uncheck Include basemap.')),15000);
              safe.onload=()=>{clearTimeout(timer);resolve();};
              safe.onerror=()=>{clearTimeout(timer);reject(Error('The basemap provider blocks image capture. Uncheck Include basemap to save the area and tile outlines.'));};
              safe.src=img.src;
            });
            const c=document.createElement('canvas');c.width=safe.naturalWidth;c.height=safe.naturalHeight;
            c.getContext('2d').drawImage(safe,0,0);copies.set(img.src,c.toDataURL('image/png'));
          }));
        }
        const canvas=await html2canvas(map,{backgroundColor:'#14232d',scale:Math.min(2,2400/rect.width,2400/rect.height),
          useCORS:true,allowTaint:false,logging:false,
          onclone:doc=>{
            const clone=doc.getElementById('map');
            clone.querySelectorAll('.leaflet-control-zoom,.leaflet-draw,.leaflet-control-layers').forEach(el=>el.remove());
            clone.querySelectorAll('img.leaflet-tile').forEach(img=>{
              if(!include||!copies.has(img.src))img.remove();else img.src=copies.get(img.src);
            });
          }});
        const credit=map.querySelector('.leaflet-control-attribution')?.textContent||'';
        await save(canvas,'als-study-area-tiles.png',[
          'ALS Downloader | Area of interest and visible tile footprints',
          document.getElementById('search_status')?.textContent||'',
          include?credit:'Basemap omitted. '+credit,
          'Dashed gold outline: area of interest. Tile colours: reported acquisition years. Country shading: catalogue availability, not survey coverage.',
          ...(document.getElementById('map_source_credits')?.textContent||'Source credit and licence not supplied.').split('\n'),
          'Source and basemap terms apply. Check uploaded boundary rights. Footprints rendered from source metadata.'
        ],status);
      }catch(e){status.textContent=e.message;}
      finally{button.disabled=false;}
    };
  });
})();
