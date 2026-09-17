(function () {
  let points = [], origin = [0, 0, 0], yaw = -.65, pitch = .8, exag = 4, zoom = 1, drag = null;
  const palette = [[68,1,84],[59,82,139],[33,145,140],[94,201,98],[253,231,37]];
  function draw() {
    const c = document.getElementById('als-cloud');
    if (!c || !c.clientWidth) return;
    const w = c.clientWidth, h = c.clientHeight, dpr = Math.min(devicePixelRatio || 1, 2);
    c.width = w*dpr; c.height = h*dpr;
    const ctx = c.getContext('2d'); ctx.scale(dpr,dpr);
    ctx.fillStyle = '#adbeca'; ctx.font = '12px system-ui';
    if (!points.length) { ctx.fillText('Upload a point cloud to begin.',20,35); return; }
    const max = [0,0,0];
    for (const p of points) for (let j=0;j<3;j++) max[j]=Math.max(max[j],p[j]);
    const scale = Math.min(w,h)*.8*zoom / Math.max(max[0],max[1],max[2]*exag,1);
    const co=Math.cos(yaw), si=Math.sin(yaw), cp=Math.cos(pitch), sp=Math.sin(pitch);
    const ordered=points.map(p=>{
      const x=p[0]-max[0]/2,y=p[1]-max[1]/2,z=(p[2]-max[2]/2)*exag;
      const rx=x*co-y*si,ry=x*si+y*co;
      return [rx*scale+w/2,(ry*cp-z*sp)*scale+h/2,ry*sp+z*cp,p[2]];
    }).sort((a,b)=>a[2]-b[2]);
    for (const p of ordered) {
      const v=Math.max(0,Math.min(3.999,4*p[3]/(max[2]||1))),i=Math.floor(v),t=v-i;
      const rgb=palette[i].map((x,j)=>Math.round(x+(palette[i+1][j]-x)*t));
      ctx.fillStyle=`rgb(${rgb.join(',')})`;ctx.fillRect(p[0],p[1],1.5,1.5);
    }
    const gradient=ctx.createLinearGradient(20,0,160,0);
    palette.forEach((p,i)=>gradient.addColorStop(i/4,`rgb(${p.join(',')})`));
    ctx.fillStyle='rgba(8,14,20,.85)';ctx.fillRect(10,h-65,180,60);
    ctx.fillStyle=gradient;ctx.fillRect(20,h-42,140,6);
    ctx.fillStyle='#c7d6df';ctx.fillText('Elevation (source units)',20,h-50);
    ctx.fillText(Number(origin[2]).toFixed(1),20,h-18);
    ctx.textAlign='right';ctx.fillText((Number(origin[2])+max[2]).toFixed(1),160,h-18);
  }
  function init() {
    const c=document.getElementById('als-cloud'); if(!c)return;
    const reset=()=>{yaw=-.65;pitch=.8;zoom=1;draw();};
    c.onpointerdown=e=>{drag=[e.clientX,e.clientY];c.setPointerCapture(e.pointerId);c.focus();};
    c.onpointermove=e=>{if(!drag)return;yaw+=(e.clientX-drag[0])*.008;pitch=Math.max(.1,Math.min(1.5,pitch+(e.clientY-drag[1])*.008));drag=[e.clientX,e.clientY];draw();};
    c.onpointerup=c.onpointercancel=()=>drag=null;
    c.onwheel=e=>{e.preventDefault();zoom=Math.max(.4,Math.min(5,zoom*Math.exp(-e.deltaY*.001)));draw();};
    c.ondblclick=reset;
    c.onkeydown=e=>{
      if(!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','+','=','-','0'].includes(e.key))return;
      e.preventDefault();
      if(e.key==='0'){reset();return;}
      if(e.key==='ArrowLeft')yaw-=.1;if(e.key==='ArrowRight')yaw+=.1;
      if(e.key==='ArrowUp')pitch=Math.min(1.5,pitch+.1);if(e.key==='ArrowDown')pitch=Math.max(.1,pitch-.1);
      if(e.key==='+'||e.key==='=')zoom=Math.min(5,zoom*1.15);if(e.key==='-')zoom=Math.max(.4,zoom/1.15);
      draw();
    };
    const compact=matchMedia('(max-width:850px)');
    function layout(){const sidebar=document.querySelector('.als-sidebar');if(sidebar)sidebar.open=!compact.matches;}
    compact.addEventListener('change',layout);layout();
    new ResizeObserver(draw).observe(c);
    Shiny.addCustomMessageHandler('als-points',data=>{points=data.points;origin=data.origin;reset();});
    Shiny.addCustomMessageHandler('als-exaggeration',value=>{exag=value;draw();});
    draw();
  }
  document.addEventListener('DOMContentLoaded',init);
})();
