(function () {
  const palettes = {
    Greyscale: [[40,48,57],[245,248,250]],
    Greens: [[247,252,245],[199,233,192],[116,196,118],[35,139,69],[0,68,27]],
    Viridis: [[68,1,84],[71,44,122],[59,82,139],[44,113,142],[33,145,140],[39,173,129],[94,201,98],[170,220,50],[253,231,37]],
    Magma: [[0,0,4],[28,16,68],[79,18,123],[129,37,129],[181,54,122],[229,80,100],[251,135,97],[254,194,135],[252,253,191]],
    Plasma: [[13,8,135],[126,3,168],[204,71,120],[248,149,64],[240,249,33]],
    Cividis: [[0,34,78],[67,78,108],[125,124,120],[188,173,108],[254,232,56]],
    Grey: [[150,150,150],[150,150,150]], Black: [[0,0,0],[0,0,0]],
    'Light purple': [[195,166,255],[195,166,255]], 'Pale yellow': [[255,231,135],[255,231,135]],
    Blue: [[75,158,255],[75,158,255]], Red: [[255,99,105],[255,99,105]],
    Cyan: [[0,220,240],[0,220,240]], Orange: [[255,150,50],[255,150,50]]
  };
  const makeLut = colors => Array.from({length:256},(_,n)=>{
    const v=n/255*(colors.length-1),i=Math.min(colors.length-2,Math.floor(v)),t=v-i;
    return 'rgb('+colors[i].map((x,j)=>Math.round(x+(colors[i+1][j]-x)*t)).join(',')+')';
  });
  // LAS source codes, not inferred land cover. Other codes keep their numbers.
  const classes = {
    0: ['Never classified', '#8895a5'], 1: ['Unclassified (land cover unknown)', '#8eb69c'],
    2: ['Ground', '#cba574'], 3: ['Low vegetation', '#c6df85'],
    4: ['Medium vegetation', '#78c679'], 5: ['High vegetation', '#28a96b'],
    6: ['Building', '#ed8b73'], 7: ['Low noise', '#cf83cf'],
    9: ['Water', '#65b9ef'], 17: ['Bridge deck', '#e3c46a'],
    18: ['High noise', '#b695ed']
  };
  const classInfo = code => code == null ? ['Classification unavailable', '#8895a5'] :
    (classes[code] || ['Class '+code, '#a8a4cf']);
  function viewer(c) {
    let points=[],origin=[0,0,0],extent=[0,0,0],initialYaw=-.65,yaw=-.65,pitch=0,exag=2,zoom=1,drag=null,palette='Greyscale',colourBy='auto',activeMode='elevation';
    let initialPitch=0,pointSize=1.8,classification=[],classColors=[],intensity=[],intensityRange=[0,0],sourceLabel="Point-cloud preview",attribution=[],unitLabel="source units (unverified)",unitNote="";
    const classLegend=document.createElement('div');
    classLegend.className='als-classification-legend';
    classLegend.setAttribute('aria-label','Source classification legend');
    classLegend.style.cssText='display:none;flex-wrap:wrap;gap:8px 18px;padding:12px 16px;background:#101b24;border:1px solid #273744;border-radius:0 0 8px 8px;color:#d8e2e9;font:12px system-ui;max-height:120px;overflow:auto';
    c.insertAdjacentElement('afterend',classLegend);
    function updateLegend(){
      classLegend.replaceChildren();
      classLegend.style.display=points.length?'flex':'none';
      if(!points.length)return;
      if(activeMode!=='classification'){
        classLegend.textContent=(colourBy==='auto'?'Automatic: ':'')+(activeMode==='intensity'?'Intensity (raw source values)':activeMode==='missing'?'Intensity unavailable; points shown in grey':'Elevation ('+unitLabel+')')+' | '+palette;
        return;
      }
      const counts=new Map();
      for(const code of classification)counts.set(code,(counts.get(code)||0)+1);
      for(const [code,count] of [...counts].sort((a,b)=>(a[0]??256)-(b[0]??256))){
        const [name,color]=classInfo(code),item=document.createElement('span'),swatch=document.createElement('span');
        swatch.style.cssText='display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:6px;background:'+color;
        item.append(swatch,document.createTextNode((code==null?'':code+' \u00b7 ')+name+' \u00b7 '+(100*count/points.length).toFixed(1)+'%'));
        classLegend.append(item);
      }
    }
    function resolveMode(){
      activeMode=colourBy==='auto'?(classification.some(x=>x!==null&&x>1)?'classification':intensity.some(x=>x!==null&&x>0)?'intensity':'elevation'):colourBy;
      if(activeMode==='intensity'&&!intensity.some(x=>x!==null))activeMode='missing';
      updateLegend();
    }
    function draw() {
      if (!c.clientWidth) return;
      const w=c.clientWidth,h=c.clientHeight,dpr=Math.min(devicePixelRatio||1,2);
      c.width=w*dpr;c.height=h*dpr;
      const ctx=c.getContext('2d');ctx.scale(dpr,dpr);
      ctx.fillStyle=activeMode!=='classification'&&palette==='Black'?'#eef2f5':'#05080c';ctx.fillRect(0,0,w,h);
      ctx.fillStyle='#adbeca';ctx.font='12px system-ui';
      if(!points.length){ctx.fillText('Select a tile to plot, or upload a local point cloud in 3D preview.',20,35);return;}
      const co=Math.cos(yaw),si=Math.sin(yaw),cp=Math.cos(pitch),sp=Math.sin(pitch);
      let xmin=Infinity,xmax=-Infinity,ymin=Infinity,ymax=-Infinity;
      const ordered=points.map((p,i)=>{
        const x=p[0]-extent[0]/2,y=p[1]-extent[1]/2,z=(p[2]-extent[2]/2)*exag;
        const rx=x*co-y*si,ry=x*si+y*co,py=ry*cp-z*sp;
        xmin=Math.min(xmin,rx);xmax=Math.max(xmax,rx);ymin=Math.min(ymin,py);ymax=Math.max(ymax,py);
        return [rx,py,ry*sp+z*cp,p[2],i];
      }).sort((a,b)=>a[2]-b[2]);
      // Fit projected bounds to the landscape canvas, with room for the legend.
      const scale=Math.min((w-60)/Math.max(xmax-xmin,1),(h-110)/Math.max(ymax-ymin,1))*zoom;
      const lut=makeLut(palettes[palette]);
      for(const p of ordered){
        const value=activeMode==='intensity'?intensity[p[4]]:p[3];
        const ratio=activeMode==='intensity'?(value-intensityRange[0])/(intensityRange[1]-intensityRange[0]||1):p[3]/(extent[2]||1);
        ctx.fillStyle=activeMode==='classification'?classColors[p[4]]:activeMode==='missing'||value===null?'#8895a5':lut[Math.max(0,Math.min(255,Math.round(255*ratio)))];
        ctx.fillRect((p[0]-(xmin+xmax)/2)*scale+w/2,(p[1]-(ymin+ymax)/2)*scale+(h-45)/2,pointSize,pointSize);
      }
      function legend(name,x,width,label){
        ctx.textAlign='left';const stops=palettes[name];const gradient=ctx.createLinearGradient(x,0,x+width,0);
        stops.forEach((p,i)=>gradient.addColorStop(i/(stops.length-1),'rgb('+p.join(',')+')'));
        ctx.fillStyle='rgba(8,14,20,.95)';ctx.fillRect(x-10,h-70,width+20,65);
        ctx.fillStyle=gradient;ctx.fillRect(x,h-40,width,7);
        ctx.fillStyle='#c7d6df';ctx.fillText(label+' '+name+' · Z ×'+exag,x,h-50);
        if(stops.every(p=>p.every((v,i)=>v===stops[0][i])))return;
        ctx.fillText((activeMode==='intensity'?intensityRange[0]:Number(origin[2])).toFixed(1),x,h-15);
        ctx.textAlign='right';ctx.fillText((activeMode==='intensity'?intensityRange[1]:Number(origin[2])+extent[2]).toFixed(1),x+width,h-15);
      }
      if(activeMode==='intensity'||activeMode==='elevation')legend(palette,20,180,activeMode==='intensity'?'Intensity':'Elevation');
    }
    function fit(){yaw=initialYaw;pitch=initialPitch;zoom=1;draw();}
    c.onpointerdown=e=>{drag=[e.clientX,e.clientY];c.setPointerCapture(e.pointerId);c.focus();};
    c.onpointermove=e=>{if(!drag)return;yaw+=(e.clientX-drag[0])*.008;pitch=Math.max(0,Math.min(1.5,pitch+(e.clientY-drag[1])*.008));drag=[e.clientX,e.clientY];draw();};
    c.onpointerup=()=>{drag=null;};
    c.onpointercancel=()=>{drag=null;};
    c.onwheel=e=>{e.preventDefault();zoom=Math.max(.4,Math.min(8,zoom*Math.exp(-e.deltaY*.001)));draw();};
    c.ondblclick=()=>fit();
    c.onkeydown=e=>{
      if(!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','+','=','-','0'].includes(e.key))return;
      e.preventDefault();if(e.key==='0'){fit();return;}
      if(e.key==='ArrowLeft')yaw-=.1;if(e.key==='ArrowRight')yaw+=.1;
      if(e.key==='ArrowUp')pitch=Math.min(1.5,pitch+.1);if(e.key==='ArrowDown')pitch=Math.max(0,pitch-.1);
      if(e.key==='+'||e.key==='=')zoom=Math.min(8,zoom*1.15);if(e.key==='-')zoom=Math.max(.4,zoom/1.15);draw();
    };
    const saveButton=c.id==='als-cloud'?document.getElementById('export_preview_png'):null;
    if(saveButton)saveButton.onclick=()=>{
      if(!points.length)return;
      window.ALSFigures.save(c,'als-point-cloud.png',[
        'ALS Downloader | '+sourceLabel,
        'Colour: '+activeMode+(colourBy==='auto'?' (automatic)':'')+(activeMode==='classification'?' | Source class colours':' | '+palette)+' | Z exaggeration '+exag+'x',
        activeMode==='classification'?'Source classification keys shown above.':classLegend.textContent,
        points.length.toLocaleString()+' sampled points. No new classification or height normalization.',unitNote,...attribution
      ],document.getElementById('preview_export_status'),activeMode==='classification'?[...classLegend.children].map(el=>({label:el.textContent,color:el.firstChild.style.backgroundColor})):[]);
    };
    const observer=new ResizeObserver(draw);observer.observe(c);draw();
    return {dispose(){observer.disconnect();classLegend.remove();},load(data){
      points=data.points;origin=data.origin;extent=[0,0,0];
      classification=points.map((_,i)=>{
        const code=Array.isArray(data.classification)?data.classification[i]:(i===0?data.classification:null);
        return Number.isInteger(code)&&code>=0&&code<=255?code:null;
      });
      intensity=points.map((_,i)=>{
        const v=Array.isArray(data.intensity)?data.intensity[i]:(i===0?data.intensity:null);
        return typeof v==='number'&&Number.isFinite(v)&&v>=0?v:null;
      });
      const validIntensity=intensity.filter(v=>v!==null);
      intensityRange=validIntensity.length?validIntensity.reduce((r,v)=>[Math.min(r[0],v),Math.max(r[1],v)],[Infinity,-Infinity]):[0,0];
      unitLabel=data.units||'source units (unverified)';unitNote=data.units_note||'Coordinate units not supplied; verify original source.';sourceLabel=data.label||'Point-cloud preview';attribution=Array.isArray(data.attribution)?data.attribution:['Source credit and licence not supplied. Check original source before publication.'];
      classColors=classification.map(code=>classInfo(code)[1]);
      if(saveButton)saveButton.disabled=!points.length;
      resolveMode();
      let sx=0,sy=0,sxx=0,syy=0,sxy=0;
      for(const p of points){for(let j=0;j<3;j++)extent[j]=Math.max(extent[j],p[j]);sx+=p[0];sy+=p[1];sxx+=p[0]*p[0];syy+=p[1]*p[1];sxy+=p[0]*p[1];}
      const n=points.length||1;
      initialYaw=-.5*Math.atan2(2*(sxy-sx*sy/n),sxx-sx*sx/n-syy+sy*sy/n);
      fit();},
      update(data){if(data.focusCentral!=null){}if(data.exaggeration!=null)exag=data.exaggeration;if(data.palette==='Classification')colourBy='classification';else if(palettes[data.palette])palette=data.palette;
        if(['auto','classification','intensity','elevation'].includes(data.colourBy))colourBy=data.colourBy;resolveMode();
        if(data.pointSize!=null)pointSize=Math.max(.7,Math.min(3,data.pointSize));
        if(data.pose){initialPitch=data.pose==='top'?0:(data.pose==='forest'?1.38:1.08);fit();}
        if(data.fit)fit();else draw();}};
  }
  // Two synced side-by-side panels: A and B always show one campaign each, sharing
  // one camera (rotation/zoom) and one profile-line selection drawn across both.
  function comparisonViewer(ca, cb) {
    let points=[],origin=[0,0,0],extent=[0,0,0],initialYaw=-.65,yaw=-.65,pitch=0,exag=1,zoom=1,drag=null;
    let groups=[],palette='Red',paletteB='Blue',showA=true,showB=true,focusCentral=true;
    let cloudMode='shared',sharedPalette='Greens';
    let initialPitch=0,pointSize=1.8,labels=[],camera=null,crs='',attribution=[];
    let ordered=[],xmin=0,xmax=0,ymin=0,ymax=0,scale=1;
    const dc=document.getElementById('als-compare-density');
    // A simple binned count of loaded Z values per campaign: how much of each
    // cloud sits at each elevation, not a fitted or modeled distribution.
    function renderDensity(){
      const densityButton=document.getElementById('export_density_png');
      if(densityButton){densityButton.disabled=!points.length;densityButton.onclick=()=>window.ALSFigures.save(dc,'als-elevation-distribution.png',['ALS Downloader | Elevation distribution',...labels,'Sampled source elevations; not a calculated difference.',...attribution]);}
      if(!dc||!dc.clientWidth)return;
      const w=dc.clientWidth,h=220,dpr=Math.min(devicePixelRatio||1,2);
      dc.width=w*dpr;dc.height=h*dpr;const g=dc.getContext('2d');g.scale(dpr,dpr);
      g.fillStyle='#05080c';g.fillRect(0,0,w,h);
      g.font='12px system-ui';
      if(!points.length){g.fillStyle='#adbeca';g.fillText('Load two clouds to see their elevation distribution.',20,30);return;}
      const za=[],zb=[];
      for(let i=0;i<points.length;i++){
        const isB=groups[i]===1;
        if(isB?showB:showA)(isB?zb:za).push(Number(points[i][2])+Number(origin[2]));
      }
      const left=56,right=16,top=34,bottom=30,pw=Math.max(10,w-left-right),ph=Math.max(10,h-top-bottom);
      const all=za.concat(zb);
      if(!all.length){g.fillStyle='#adbeca';g.fillText('Both clouds are hidden. Show A or B to see elevations.',20,30);return;}
      let zmin=Math.min(...all),zmax=Math.max(...all);
      if(!isFinite(zmin)||!isFinite(zmax))return;
      if(zmin===zmax){zmin-=1;zmax+=1;}
      const bins=32,binWidth=(zmax-zmin)/bins;
      function hist(arr){const counts=new Array(bins).fill(0);
        for(const z of arr){let i=Math.floor((z-zmin)/binWidth);if(i<0)i=0;if(i>=bins)i=bins-1;counts[i]++;}
        return counts;}
      const ha=hist(za),hb=hist(zb),maxCount=Math.max(1,...ha,...hb);
      function bars(counts,color){g.fillStyle=color;
        for(let i=0;i<bins;i++){const bh=counts[i]/maxCount*ph;
          g.fillRect(left+i/bins*pw,top+ph-bh,pw/bins-1,bh);}}
      g.save();g.beginPath();g.rect(left,top,pw,ph);g.clip();g.globalAlpha=.55;
      bars(ha,'#ff6369');bars(hb,'#4b9eff');g.globalAlpha=1;g.restore();
      g.strokeStyle='#33404e';g.lineWidth=1;g.beginPath();
      g.moveTo(left,top);g.lineTo(left,top+ph);g.lineTo(left+pw,top+ph);g.stroke();
      g.fillStyle='#c7d6df';g.textAlign='center';g.font='11px system-ui';
      g.fillText(zmin.toFixed(1)+' m',left,top+ph+16);g.fillText(zmax.toFixed(1)+' m',left+pw,top+ph+16);
      g.textAlign='left';g.font='12px system-ui';
      const mean=arr=>arr.reduce((a,b)=>a+b,0)/arr.length;
      g.fillStyle='#ff9fa3';
      g.fillText('A: n='+za.length+(za.length?', mean '+mean(za).toFixed(2)+' m':' (hidden or none loaded)'),left,20);
      g.fillStyle='#9fc8ff';
      g.fillText('B: n='+zb.length+(zb.length?', mean '+mean(zb).toFixed(2)+' m':' (hidden or none loaded)'),left+Math.min(260,pw/2+20),20);
    }
    const profile = window.ALSProfile ? window.ALSProfile([ca,cb],
      ()=>({points,groups,origin,extent,labels,crs,attribution,palette,paletteB,cloudMode,sharedPalette,showA,showB,focusCentral,palettes,camera,pitch,exag}),
      drawBoth, ()=>{pitch=0;yaw=0;zoom=1;drawBoth();}) : null;
    function project(){
      if(!points.length){ordered=[];return;}
      const co=Math.cos(yaw),si=Math.sin(yaw),cp=Math.cos(pitch),sp=Math.sin(pitch);
      xmin=Infinity;xmax=-Infinity;ymin=Infinity;ymax=-Infinity;
      ordered=points.map((p,index)=>{
        const x=p[0]-extent[0]/2,y=p[1]-extent[1]/2,z=(p[2]-extent[2]/2)*exag;
        const rx=x*co-y*si,ry=x*si+y*co,py=ry*cp-z*sp;
        xmin=Math.min(xmin,rx);xmax=Math.max(xmax,rx);ymin=Math.min(ymin,py);ymax=Math.max(ymax,py);
        return [rx,py,ry*sp+z*cp,p[2],groups[index]||0];
      }).sort((a,b)=>a[2]-b[2]);
      // Shared bounds across both campaigns keep A and B on the same scale, so
      // their relative height and position stay comparable side by side.
      if(focusCentral&&ordered.length>100){
        const xs=ordered.map(p=>p[0]).sort((a,b)=>a-b),ys=ordered.map(p=>p[1]).sort((a,b)=>a-b);
        const lo=Math.floor((ordered.length-1)*.01),hi=Math.ceil((ordered.length-1)*.99);
        xmin=xs[lo];xmax=xs[hi];ymin=ys[lo];ymax=ys[hi];
      }
    }
    function panel(c, groupFilter, activePalette){
      if(!c.clientWidth) return null;
      const w=c.clientWidth,h=c.clientHeight,dpr=Math.min(devicePixelRatio||1,2);
      c.width=w*dpr;c.height=h*dpr;
      const ctx=c.getContext('2d');ctx.scale(dpr,dpr);
      ctx.fillStyle=activePalette==='Black'?'#eef2f5':'#05080c';ctx.fillRect(0,0,w,h);
      ctx.fillStyle='#adbeca';ctx.font='12px system-ui';
      if(!points.length){ctx.fillText('Choose two overlapping clouds to view together.',20,35);return ctx;}
      const visible=groupFilter===0?showA:showB;
      if(visible){
        const lut=makeLut(palettes[activePalette]);
        for(const p of ordered){
          if(p[4]!==groupFilter)continue;
          ctx.fillStyle=lut[Math.round(255*p[3]/(extent[2]||1))];
          ctx.fillRect((p[0]-(xmin+xmax)/2)*scale+w/2,(p[1]-(ymin+ymax)/2)*scale+(h-45)/2,pointSize,pointSize);
        }
      }
      const legendWidth=Math.min(180,Math.max(40,w-40));
      const stops=palettes[activePalette];const gradient=ctx.createLinearGradient(20,0,20+legendWidth,0);
      stops.forEach((s,i)=>gradient.addColorStop(i/(stops.length-1),'rgb('+s.join(',')+')'));
      ctx.fillStyle='rgba(8,14,20,.95)';ctx.fillRect(10,h-70,legendWidth+20,65);
      ctx.fillStyle=gradient;ctx.fillRect(20,h-40,legendWidth,7);
      ctx.fillStyle='#c7d6df';
      ctx.fillText(activePalette+' · Z ×'+exag+(visible?'':' (hidden)')+(labels[groupFilter]?(' | '+labels[groupFilter]):''),20,h-50);
      ctx.fillText(Number(origin[2]).toFixed(1),20,h-15);
      ctx.textAlign='right';ctx.fillText((Number(origin[2])+extent[2]).toFixed(1),20+legendWidth,h-15);
      ctx.textAlign='left';
      return ctx;
    }
    function drawBoth(){
      project();
      const w=Math.max(ca.clientWidth||1,1),h=Math.max(ca.clientHeight||1,1);
      scale=points.length?Math.min((w-60)/Math.max(xmax-xmin,1),(h-110)/Math.max(ymax-ymin,1))*zoom:1;
      camera={scale,cx:(xmin+xmax)/2,cy:(ymin+ymax)/2,co:Math.cos(yaw),si:Math.sin(yaw)};
      const ctxA=panel(ca,0,cloudMode==='shared'?sharedPalette:palette),ctxB=panel(cb,1,cloudMode==='shared'?sharedPalette:paletteB);
      if(profile){
        if(ctxA)profile.renderOverlay(ctxA,ca);
        if(ctxB)profile.renderOverlay(ctxB,cb);
        profile.renderChart();
      }
      renderDensity();
    }
    function fit(){yaw=initialYaw;pitch=initialPitch;zoom=1;drawBoth();}
    function attach(c){
      c.onpointerdown=e=>{if(profile?.pointer('down',e))return;drag=[e.clientX,e.clientY];c.setPointerCapture(e.pointerId);c.focus();};
      c.onpointermove=e=>{if(profile?.pointer('move',e))return;if(!drag)return;yaw+=(e.clientX-drag[0])*.008;pitch=Math.max(0,Math.min(1.5,pitch+(e.clientY-drag[1])*.008));drag=[e.clientX,e.clientY];drawBoth();};
      c.onpointerup=e=>{if(profile?.pointer('up',e))return;drag=null;};
      c.onpointercancel=()=>{drag=null;profile?.cancel();};
      c.onwheel=e=>{e.preventDefault();zoom=Math.max(.4,Math.min(8,zoom*Math.exp(-e.deltaY*.001)));drawBoth();};
      c.ondblclick=()=>{if(!profile?.isDrawing())fit();};
      c.onkeydown=e=>{
        if(e.key==='Escape'){profile?.cancel();return;}
        if(profile?.isDrawing())return;
        if(!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','+','=','-','0'].includes(e.key))return;
        e.preventDefault();if(e.key==='0'){fit();return;}
        if(e.key==='ArrowLeft')yaw-=.1;if(e.key==='ArrowRight')yaw+=.1;
        if(e.key==='ArrowUp')pitch=Math.min(1.5,pitch+.1);if(e.key==='ArrowDown')pitch=Math.max(0,pitch-.1);
        if(e.key==='+'||e.key==='=')zoom=Math.min(8,zoom*1.15);if(e.key==='-')zoom=Math.max(.4,zoom/1.15);drawBoth();
      };
      const observer=new ResizeObserver(drawBoth);observer.observe(c);
      return observer;
    }
    const obsA=attach(ca),obsB=attach(cb);
    drawBoth();
    return {dispose(){obsA.disconnect();obsB.disconnect();},load(data){
      points=data.points;origin=data.origin;groups=data.groups||[];labels=data.labels||[];crs=data.crs||'';attribution=Array.isArray(data.attribution)?data.attribution:['Source credit and licence not supplied. Check original source before publication.'];extent=[0,0,0];profile?.reset();
      let sx=0,sy=0,sxx=0,syy=0,sxy=0;
      for(const p of points){for(let j=0;j<3;j++)extent[j]=Math.max(extent[j],p[j]);sx+=p[0];sy+=p[1];sxx+=p[0]*p[0];syy+=p[1]*p[1];sxy+=p[0]*p[1];}
      const n=points.length||1;
      initialYaw=-.5*Math.atan2(2*(sxy-sx*sy/n),sxx-sx*sx/n-syy+sy*sy/n);
      fit();},
      update(data){if(data.focusCentral!=null)focusCentral=data.focusCentral;if(data.exaggeration!=null)exag=data.exaggeration;if(palettes[data.palette])palette=data.palette;
        if(['shared','campaign'].includes(data.cloudMode))cloudMode=data.cloudMode;
        if(palettes[data.sharedPalette])sharedPalette=data.sharedPalette;
        if(data.pointSize!=null)pointSize=Math.max(.7,Math.min(3,data.pointSize));
        if(data.pose){initialPitch=data.pose==='top'?0:(data.pose==='forest'?1.38:1.08);fit();}
        if(palettes[data.paletteB])paletteB=data.paletteB;if(data.showA!=null)showA=data.showA;if(data.showB!=null)showB=data.showB;
        if(data.fit)fit();else drawBoth();}};
  }
  document.addEventListener('DOMContentLoaded',()=>{
    const views={};for(const id of ['als-cloud','als-tile-cloud']){const c=document.getElementById(id);if(c)views[id]=viewer(c);}
    const ca=document.getElementById('als-compare-cloud-a'),cb=document.getElementById('als-compare-cloud-b');
    if(ca&&cb)views['als-compare-cloud']=comparisonViewer(ca,cb);
    const compact=matchMedia('(max-width:850px)');
    function layout(){const sidebar=document.querySelector('.als-sidebar');if(sidebar)sidebar.open=!compact.matches;}
    compact.addEventListener('change',layout);layout();
    Shiny.addCustomMessageHandler('als-points',data=>views[data.target||'als-cloud']?.load(data));
    Shiny.addCustomMessageHandler('als-view',data=>views[data.target]?.update(data));
  });
})();
