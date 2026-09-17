(function () {
  const palettes = {
    Viridis: [[68,1,84],[71,44,122],[59,82,139],[44,113,142],[33,145,140],[39,173,129],[94,201,98],[170,220,50],[253,231,37]],
    Magma: [[0,0,4],[28,16,68],[79,18,123],[129,37,129],[181,54,122],[229,80,100],[251,135,97],[254,194,135],[252,253,191]],
    Plasma: [[13,8,135],[126,3,168],[204,71,120],[248,149,64],[240,249,33]],
    Cividis: [[0,34,78],[67,78,108],[125,124,120],[188,173,108],[254,232,56]],
    Grey: [[150,150,150],[150,150,150]], Black: [[0,0,0],[0,0,0]],
    Cyan: [[0,220,240],[0,220,240]], Orange: [[255,150,50],[255,150,50]]
  };
  function viewer(c) {
    const comparison=c.id==='als-compare-cloud';
    let points=[],origin=[0,0,0],extent=[0,0,0],initialYaw=-.65,yaw=-.65,pitch=1.08,exag=2,zoom=1,drag=null,palette='Viridis';
    let groups=[],paletteB='Magma',showA=true,showB=true,focusCentral=true;
    let initialPitch=1.08,pointSize=1.8;
    if(comparison){palette='Grey';paletteB='Black';exag=1;}
    function draw() {
      if (!c.clientWidth) return;
      const w=c.clientWidth,h=c.clientHeight,dpr=Math.min(devicePixelRatio||1,2);
      c.width=w*dpr;c.height=h*dpr;
      const ctx=c.getContext('2d');ctx.scale(dpr,dpr);
      if(comparison){ctx.fillStyle='#ffffff';ctx.fillRect(0,0,w,h);}
      ctx.fillStyle=comparison?'#424242':'#adbeca';ctx.font='12px system-ui';
      if(!points.length){ctx.fillText(comparison?'Choose two overlapping clouds to view together.':'Select a tile to plot, or upload a local point cloud in 3D preview.',20,35);return;}
      const co=Math.cos(yaw),si=Math.sin(yaw),cp=Math.cos(pitch),sp=Math.sin(pitch);
      let xmin=Infinity,xmax=-Infinity,ymin=Infinity,ymax=-Infinity;
      const ordered=points.map((p,index)=>{
        const x=p[0]-extent[0]/2,y=p[1]-extent[1]/2,z=(p[2]-extent[2]/2)*exag;
        const rx=x*co-y*si,ry=x*si+y*co,py=ry*cp-z*sp;
        xmin=Math.min(xmin,rx);xmax=Math.max(xmax,rx);ymin=Math.min(ymin,py);ymax=Math.max(ymax,py);
        return [rx,py,ry*sp+z*cp,p[2],groups[index]||0];
      }).sort((a,b)=>a[2]-b[2]);
      // Camera framing only: retain every point, but let users avoid extreme
      // returns compressing the whole cloud into a tiny area of the viewport.
      if(comparison && focusCentral && ordered.length>100){
        const xs=ordered.map(p=>p[0]).sort((a,b)=>a-b),ys=ordered.map(p=>p[1]).sort((a,b)=>a-b);
        const lo=Math.floor((ordered.length-1)*.01),hi=Math.ceil((ordered.length-1)*.99);
        xmin=xs[lo];xmax=xs[hi];ymin=ys[lo];ymax=ys[hi];
      }
      // Fit projected bounds to the landscape canvas, with room for the legend.
      const scale=Math.min((w-60)/Math.max(xmax-xmin,1),(h-110)/Math.max(ymax-ymin,1))*zoom;
      const colors=palettes[palette];
      const makeLut=colors=>Array.from({length:256},(_,n)=>{
        const v=n/255*(colors.length-1),i=Math.min(colors.length-2,Math.floor(v)),t=v-i;
        return 'rgb('+colors[i].map((x,j)=>Math.round(x+(colors[i+1][j]-x)*t)).join(',')+')';
      });
      const lut=makeLut(colors),lutB=makeLut(palettes[paletteB]);
      for(const p of ordered){
        if((p[4]===0&&!showA)||(p[4]===1&&!showB))continue;
        ctx.fillStyle=(p[4]===1?lutB:lut)[Math.round(255*p[3]/(extent[2]||1))];
        ctx.fillRect((p[0]-(xmin+xmax)/2)*scale+w/2,(p[1]-(ymin+ymax)/2)*scale+(h-45)/2,pointSize,pointSize);
      }
      function legend(name,x,width,label){
        ctx.textAlign='left';const stops=palettes[name];const gradient=ctx.createLinearGradient(x,0,x+width,0);
        stops.forEach((p,i)=>gradient.addColorStop(i/(stops.length-1),'rgb('+p.join(',')+')'));
        ctx.fillStyle=comparison?'rgba(255,255,255,.95)':'rgba(8,14,20,.9)';ctx.fillRect(x-10,h-70,width+20,65);
        ctx.fillStyle=gradient;ctx.fillRect(x,h-40,width,7);
        ctx.fillStyle=comparison?'#333333':'#c7d6df';ctx.fillText(label+' '+name+' · Z ×'+exag,x,h-50);
        if(stops.every(p=>p.every((v,i)=>v===stops[0][i])))return;
        ctx.fillText(Number(origin[2]).toFixed(1),x,h-15);
        ctx.textAlign='right';ctx.fillText((Number(origin[2])+extent[2]).toFixed(1),x+width,h-15);
      }
      const legendWidth=groups.length?Math.min(180,(w-60)/2):180;
      legend(palette,20,legendWidth,groups.length?'A':'Elevation');
      if(groups.length)legend(paletteB,w-20-legendWidth,legendWidth,'B');
      if(groups.length){ctx.textAlign='left';ctx.fillStyle=comparison?'#333333':'#e0eaf0';ctx.fillText('A: '+palette+(showA?'':' (hidden)')+' | B: '+paletteB+(showB?'':' (hidden)'),20,20);}
    }
    function fit(){yaw=initialYaw;pitch=initialPitch;zoom=1;draw();}
    c.onpointerdown=e=>{drag=[e.clientX,e.clientY];c.setPointerCapture(e.pointerId);c.focus();};
    c.onpointermove=e=>{if(!drag)return;yaw+=(e.clientX-drag[0])*.008;pitch=Math.max(.1,Math.min(1.5,pitch+(e.clientY-drag[1])*.008));drag=[e.clientX,e.clientY];draw();};
    c.onpointerup=c.onpointercancel=()=>drag=null;
    c.onwheel=e=>{e.preventDefault();zoom=Math.max(.4,Math.min(8,zoom*Math.exp(-e.deltaY*.001)));draw();};
    c.ondblclick=fit;
    c.onkeydown=e=>{
      if(!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','+','=','-','0'].includes(e.key))return;
      e.preventDefault();if(e.key==='0'){fit();return;}
      if(e.key==='ArrowLeft')yaw-=.1;if(e.key==='ArrowRight')yaw+=.1;
      if(e.key==='ArrowUp')pitch=Math.min(1.5,pitch+.1);if(e.key==='ArrowDown')pitch=Math.max(.1,pitch-.1);
      if(e.key==='+'||e.key==='=')zoom=Math.min(8,zoom*1.15);if(e.key==='-')zoom=Math.max(.4,zoom/1.15);draw();
    };
    const observer=new ResizeObserver(draw);observer.observe(c);draw();
    return {dispose(){observer.disconnect();},load(data){
      points=data.points;origin=data.origin;groups=data.groups||[];extent=[0,0,0];
      let sx=0,sy=0,sxx=0,syy=0,sxy=0;
      for(const p of points){for(let j=0;j<3;j++)extent[j]=Math.max(extent[j],p[j]);sx+=p[0];sy+=p[1];sxx+=p[0]*p[0];syy+=p[1]*p[1];sxy+=p[0]*p[1];}
      const n=points.length||1;
      initialYaw=-.5*Math.atan2(2*(sxy-sx*sy/n),sxx-sx*sx/n-syy+sy*sy/n);
      fit();},
      update(data){if(data.focusCentral!=null)focusCentral=data.focusCentral;if(data.exaggeration!=null)exag=data.exaggeration;if(palettes[data.palette])palette=data.palette;
        if(data.pointSize!=null)pointSize=Math.max(.7,Math.min(3,data.pointSize));
        if(data.pose){initialPitch=data.pose==='forest'?1.38:1.08;fit();}
        if(palettes[data.paletteB])paletteB=data.paletteB;if(data.showA!=null)showA=data.showA;if(data.showB!=null)showB=data.showB;
        if(data.fit)fit();else draw();}};
  }
  document.addEventListener('DOMContentLoaded',()=>{
    const views={};for(const id of ['als-cloud','als-tile-cloud','als-compare-cloud']){const c=document.getElementById(id);if(c)views[id]=viewer(c);}
    const compact=matchMedia('(max-width:850px)');
    function layout(){const sidebar=document.querySelector('.als-sidebar');if(sidebar)sidebar.open=!compact.matches;}
    compact.addEventListener('change',layout);layout();
    Shiny.addCustomMessageHandler('als-points',data=>views[data.target||'als-cloud']?.load(data));
    Shiny.addCustomMessageHandler('als-view',data=>views[data.target]?.update(data));
  });
})();
