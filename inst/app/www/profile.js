/* Visual cross-sections of the displayed sample only. No surface fitting,
 * registration, interpolation or between-campaign difference calculation. */
(function () {
  function section(points,groups,start,end,width){
    const dx=end[0]-start[0],dy=end[1]-start[1],length=Math.hypot(dx,dy);
    if(length<.2)return [];
    const ux=dx/length,uy=dy/length,selected=[];
    points.forEach((p,i)=>{const x=p[0]-start[0],y=p[1]-start[1],d=x*ux+y*uy;
      if(d>=0&&d<=length&&Math.abs(-x*uy+y*ux)<=width/2)selected.push([d,p[2],groups[i]||0]);});
    return selected;
  }
  window.ALSProfileSection=section;
  // canvases: the two synced 3D panels (A, B). Drawing on either one places the
  // same world-space line and shows on both; the chart below is a single shared output.
  window.ALSProfile = function (canvases, state, redraw, planView) {
    const byId=id=>document.getElementById(id), panel=byId('profile_panel'), canvas=byId('als-compare-profile');
    if(!panel || !canvas)return null;
    let active=false,start=null,end=null,press=null,strip=2;
    const hint=byId('profile_hint');
    function enable(id,yes){if(byId(id))byId(id).disabled=!yes;}
    function setCursor(v){canvases.forEach(c=>c.style.cursor=v);}
    function reset(){active=false;start=end=press=null;panel.hidden=true;setCursor('grab');
      for(const id of ['profile_clear','export_profile','export_combined'])enable(id,false);
      const ready=state().points.length>0;enable('profile_draw',ready);enable('profile_3d',ready);enable('export_cloud',ready);
      hint.textContent=ready?'Draw profile line: choose two endpoints in plan view (in either panel), or drag. The profile uses displayed points only.':'Load two clouds to draw a profile.';}
    function cancel(){active=false;press=null;if(!end)start=null;setCursor('grab');hint.textContent='Drawing stopped. Choose Draw profile line to start again.';redraw();}
    byId('profile_draw').onclick=()=>{reset();active=true;setCursor('crosshair');planView();canvases[0].focus();hint.textContent='Plan view: click start then end (in either panel), or drag a line. Escape cancels.';};
    byId('profile_clear').onclick=()=>{reset();redraw();};
    byId('profile_3d').onclick=()=>{if(active)cancel();canvases[0].ondblclick();};
    byId('profile_width').onchange=e=>{const n=Number(e.target.value);strip=Number.isFinite(n)?Math.max(.2,Math.min(100,n)):2;e.target.value=strip;redraw();};
    function world(e){const s=state(),m=s.camera,target=e.currentTarget,r=target.getBoundingClientRect();
      if(!m)return null;
      const w=target.clientWidth,h=target.clientHeight;
      const rx=(e.clientX-r.left-w/2)/m.scale+m.cx,ry=(e.clientY-r.top-(h-45)/2)/m.scale+m.cy;
      return [rx*m.co+ry*m.si+s.extent[0]/2,-rx*m.si+ry*m.co+s.extent[1]/2];}
    function finish(p){if(!p||!start)return;const length=Math.hypot(p[0]-start[0],p[1]-start[1]);
      if(length<.2){hint.textContent='Choose endpoints at least 0.2 m apart.';return;}
      end=p;active=false;press=null;setCursor('grab');panel.hidden=false;enable('profile_clear',true);enable('export_profile',true);enable('export_combined',true);redraw();}
    function pointer(kind,e){if(!active)return false;
      e.preventDefault();
      if(kind==='down'){e.currentTarget.focus();e.currentTarget.setPointerCapture(e.pointerId);
        if(start){finish(world(e));return true;}
        start=world(e);press=[e.clientX,e.clientY];redraw();}
      if(kind==='up'&&press){if(Math.hypot(e.clientX-press[0],e.clientY-press[1])>5)finish(world(e));press=null;}
      return true;}
    function color(name,z,s){const stops=s.palettes[name],v=Math.max(0,Math.min(1,z/(s.extent[2]||1)))*(stops.length-1),i=Math.min(stops.length-2,Math.floor(v)),t=v-i;
      return 'rgb('+stops[i].map((x,j)=>Math.round(x+(stops[i+1][j]-x)*t)).join(',')+')';}
    function screen(p,s,target){const m=s.camera,x=p[0]-s.extent[0]/2,y=p[1]-s.extent[1]/2;
      const w=target.clientWidth,h=target.clientHeight;
      return [(x*m.co-y*m.si-m.cx)*m.scale+w/2,(x*m.si+y*m.co-m.cy)*m.scale+(h-45)/2];}
    // Draws the plan-view line/strip overlay onto one panel's own 3D canvas.
    function renderOverlay(ctx,target){const s=state();if(!start)return;
      // A plan view provides an unambiguous horizontal transect independent of Z.
      if(s.pitch!==0)return;
      const a=screen(start,s,target);ctx.save();ctx.strokeStyle='#f4f8fc';ctx.lineWidth=1.5;ctx.setLineDash([6,4]);
      if(end){const b=screen(end,s,target);ctx.beginPath();ctx.moveTo(...a);ctx.lineTo(...b);ctx.stroke();
        const dx=b[0]-a[0],dy=b[1]-a[1],l=Math.hypot(dx,dy),off=strip*s.camera.scale/2;
        ctx.fillStyle='rgba(220,233,255,.12)';ctx.beginPath();ctx.moveTo(a[0]-dy/l*off,a[1]+dx/l*off);ctx.lineTo(b[0]-dy/l*off,b[1]+dx/l*off);ctx.lineTo(b[0]+dy/l*off,b[1]-dx/l*off);ctx.lineTo(a[0]+dy/l*off,a[1]-dx/l*off);ctx.closePath();ctx.fill();
        ctx.fillStyle='#ffffff';ctx.fillText('End',b[0]+5,b[1]-7);}
      ctx.fillStyle='#ffffff';ctx.fillText('Start',a[0]+5,a[1]-7);ctx.restore();}
    // Draws the shared distance/elevation chart once, below both panels.
    function renderChart(){const s=state();if(!start||!end)return;
      const length=Math.hypot(end[0]-start[0],end[1]-start[1]);
      const selected=section(s.points,s.groups,start,end,strip);
      const w=canvas.clientWidth||canvases[0].clientWidth,h=300,dpr=Math.min(devicePixelRatio||1,2);
      canvas.width=w*dpr;canvas.height=h*dpr;const g=canvas.getContext('2d');g.scale(dpr,dpr);g.fillStyle=(s.palette==='Black'||s.paletteB==='Black')?'#eef2f5':'#05080c';g.fillRect(0,0,w,h);
      const left=70,right=20,top=42,bottom=52,pw=w-left-right,ph=h-top-bottom;
      let zs=selected.map(p=>p[1]).sort((a,b)=>a-b),zmin=zs[0]??0,zmax=zs.at(-1)??s.extent[2];
      if(s.focusCentral&&zs.length>100){zmin=zs[Math.floor((zs.length-1)*.01)];zmax=zs[Math.ceil((zs.length-1)*.99)];}
      const pad=Math.max(.5,(zmax-zmin)*.05);zmin-=pad;zmax+=pad;
      g.font='12px system-ui';g.fillStyle=(s.palette==='Black'||s.paletteB==='Black')?'#243542':'#e0eaf0';g.fillText('A: '+s.palette+'  |  B: '+s.paletteB, left,20);
      g.strokeStyle='#33404e';g.lineWidth=1;
      for(let i=0;i<=4;i++){const x=left+pw*i/4,y=top+ph*i/4;g.beginPath();g.moveTo(x,top);g.lineTo(x,top+ph);g.moveTo(left,y);g.lineTo(left+pw,y);g.stroke();
        g.fillStyle=(s.palette==='Black'||s.paletteB==='Black')?'#243542':'#c7d6df';g.textAlign='center';g.fillText((length*i/4).toFixed(1),x,top+ph+19);
        g.textAlign='right';g.fillText((s.origin[2]+zmax-(zmax-zmin)*i/4).toFixed(1),left-8,y+4);}
      g.save();g.beginPath();g.rect(left,top,pw,ph);g.clip();
      for(const p of selected){if((p[2]===0&&!s.showA)||(p[2]===1&&!s.showB))continue;
        g.fillStyle=color(p[2]===1?s.paletteB:s.palette,p[1],s);g.globalAlpha=.9;
        g.fillRect(left+p[0]/length*pw-1,top+(zmax-p[1])/(zmax-zmin)*ph-1,3,3);}
      g.restore();g.fillStyle=(s.palette==='Black'||s.paletteB==='Black')?'#243542':'#c7d6df';g.textAlign='center';g.fillText('Distance from line start (m)',left+pw/2,h-10);
      g.save();g.translate(16,top+ph/2);g.rotate(-Math.PI/2);g.fillText('Elevation (m)',0,0);g.restore();
      const hasA=selected.some(p=>p[2]===0),hasB=selected.some(p=>p[2]===1);
      hint.textContent='Profile strip: '+strip+' m wide, shown in both panels. '+(!hasA||!hasB?'No sampled points from '+(!hasA&&!hasB?'either cloud':!hasA?'A':'B')+' in this strip. Widen or redraw it. ':'')+'Sampled points only; gaps are not interpolated. '+(s.focusCentral?'Central 98% elevation framing; disable focus to show all elevations.':'All sampled elevations shown.');
      canvas.dataset.hasA=String(hasA);canvas.dataset.hasB=String(hasB);
    }
    function exportFigure(kind){const s=state();if(!s.points.length||(kind!=='cloud'&&!end))return;
      const sources=kind==='both'?[canvases[0],canvases[1],canvas]:kind==='profile'?[canvas]:canvases;
      const width=1600,scaled=sources.map(c=>({c,h:Math.round(c.height/c.width*width)}));
      const out=document.createElement('canvas');out.width=width;out.height=scaled.reduce((v,x)=>v+x.h,0);
      const g=out.getContext('2d');g.fillStyle='#05080c';g.fillRect(0,0,out.width,out.height);let y=0;
      for(const item of scaled){g.drawImage(item.c,0,y,width,item.h);y+=item.h;}
      g.fillStyle='#e0eaf0';g.font='18px sans-serif';
      const lines=['ALSdownloadeR | Visual overlay of sampled points; no calculated change.',
        'A: '+(s.labels[0]||'Source A')+' | '+s.palette+(s.showA?'':' (hidden)'),
        'B: '+(s.labels[1]||'Source B')+' | '+s.paletteB+(s.showB?'':' (hidden)'),
        'Cloud views: '+(s.cloudMode==='shared'?s.sharedPalette+' | shared source elevation scale':'source colours')+'. Profile colours identify A and B.',
        'Vertical scale factor: '+s.exag+'x. Profile axes show distance and elevation in metres.',
        'CRS: '+(s.crs||'See original source metadata')+'. Vertical reference is supplied by the provider; no alignment applied.',
        end?'Strip width: '+strip+' m. Start XY: '+start.map((v,i)=>(v+s.origin[i]).toFixed(2)).join(', ')+'; end XY: '+end.map((v,i)=>(v+s.origin[i]).toFixed(2)).join(', '):'No profile selected.',
        'Framing: '+(s.focusCentral?'central 98%':'all points')+'. Cite original surveys; this figure is not evidence of quantified change.',...s.attribution];
      window.ALSFigures.save(out,'als-comparison-'+kind+'.png',lines);}
    byId('export_cloud').onclick=()=>exportFigure('cloud');byId('export_profile').onclick=()=>exportFigure('profile');if(byId('export_combined'))byId('export_combined').onclick=()=>exportFigure('both');
    return {renderOverlay,renderChart,reset,cancel,pointer,isDrawing:()=>active};
  };
})();
