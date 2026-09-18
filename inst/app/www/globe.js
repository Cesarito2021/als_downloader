/* Orthographic globe rendered from bundled Natural Earth / World Atlas outlines.
   No proprietary globe imagery, external globe service or location tracking. */
document.addEventListener('DOMContentLoaded', async () => {
  const canvas=document.getElementById('als-globe');if(!canvas)return;
  const ctx=canvas.getContext('2d'), W=2048,H=1024;
  const texture=document.createElement('canvas');texture.width=W;texture.height=H;
  const t=texture.getContext('2d');t.fillStyle='#307fa2';t.fillRect(0,0,W,H);
  // A subtle geographic grid gives rotation a readable geographic frame.
  t.strokeStyle='rgba(184,223,238,0.15)';t.lineWidth=1;t.beginPath();
  for(let x=0;x<W;x+=W/12){t.moveTo(x,0);t.lineTo(x,H);}
  for(let y=H/6;y<H;y+=H/6){t.moveTo(0,y);t.lineTo(W,y);}t.stroke();
  const countries=new Set(canvas.dataset.countries.split(',').map(Number));
  const implemented=new Set((canvas.dataset.implemented||'').split(',').filter(Boolean).map(Number));
  let pixels,lon=-65,lat=18,drag=null,pending=false;
  function ring(coords,shift){
    let prev=coords[0][0];const points=coords.map(([raw,y])=>{let x=raw;while(x-prev>180)x-=360;while(x-prev< -180)x+=360;prev=x;return [x,y];});
    const first=points[0],last=points[points.length-1];
    if(Math.abs(last[0]-first[0])>180){const pole=first[1]<0?-90:90;points.push([last[0],pole],[first[0],pole]);}
    points.forEach(([x,y],i)=>{const a=(x+shift+180)*W/360,b=(90-y)*H/180;i?t.lineTo(a,b):t.moveTo(a,b);});t.closePath();
  }
  try{
    const response=await fetch('als-data/world-countries.geojson');if(!response.ok)throw Error('Map unavailable');
    const world=await response.json();
    for(const f of world.features){
      const polys=f.geometry.type==='Polygon'?[f.geometry.coordinates]:f.geometry.coordinates;
      for(const poly of polys)for(const shift of [-360,0,360]){t.beginPath();poly.forEach(coords=>ring(coords,shift));t.fillStyle='#c4d5b7';t.fill('evenodd');
        if(implemented.has(Number(f.id))){t.fillStyle='rgba(220,38,38,0.55)';t.fill('evenodd');}
        else if(countries.has(Number(f.id))){t.fillStyle='rgba(234,179,8,0.55)';t.fill('evenodd');}
        t.strokeStyle='#6c958b';t.lineWidth=.65;t.stroke();}
    }
    pixels=t.getImageData(0,0,W,H).data;canvas.dataset.ready='true';draw();
  }catch(e){document.getElementById('globe_status').textContent='Globe unavailable. Open the map to continue.';}
  function draw(){
    if(!pixels||!canvas.clientWidth)return;
    const w=Math.min(900,Math.round(canvas.clientWidth)),h=canvas.clientHeight;
    canvas.width=w;canvas.height=h;ctx.fillStyle='#050b12';ctx.fillRect(0,0,w,h);
    for(let i=0;i<95;i++){ctx.fillStyle=i%4?'#304252':'#718494';ctx.fillRect((i*137.51)%w,(i*71.13)%h,1,1);}
    const r=Math.min(w*.43,h*.435),cx=w/2,cy=h/2,phi=lat*Math.PI/180,lambda=lon*Math.PI/180;
    const glow=ctx.createRadialGradient(cx,cy,r*.9,cx,cy,r*1.1);glow.addColorStop(0,'#559ec480');glow.addColorStop(1,'#559ec400');ctx.fillStyle=glow;ctx.fillRect(0,0,w,h);
    const img=ctx.getImageData(0,0,w,h),out=img.data;
    for(let y=Math.max(0,Math.floor(cy-r));y<Math.min(h,cy+r);y++)for(let x=Math.max(0,Math.floor(cx-r));x<Math.min(w,cx+r);x++){
      const nx=(x-cx)/r,ny=(cy-y)/r,q=nx*nx+ny*ny;if(q>1)continue;
      const z=Math.sqrt(1-q),p=Math.asin(z*Math.sin(phi)+ny*Math.cos(phi)),l=lambda+Math.atan2(nx,z*Math.cos(phi)-ny*Math.sin(phi));
      const tx=Math.floor(((l/(2*Math.PI)+.5)%1+1)%1*W),ty=Math.min(H-1,Math.floor((.5-p/Math.PI)*H));
      const a=(ty*W+tx)*4,b=(y*w+x)*4,light=.55+.45*Math.max(0,z*.88-nx*.3+ny*.22);
      for(let k=0;k<3;k++)out[b+k]=pixels[a+k]*light;out[b+3]=255;
    }
    ctx.putImageData(img,0,0);ctx.beginPath();ctx.arc(cx,cy,r,0,Math.PI*2);ctx.strokeStyle='#93cbd37f';ctx.lineWidth=2;ctx.stroke();
  }
  function redraw(){if(pending)return;pending=true;requestAnimationFrame(()=>{pending=false;draw();});}
  canvas.onpointerdown=e=>{drag=[e.clientX,e.clientY];canvas.setPointerCapture(e.pointerId);canvas.focus();};
  canvas.onpointermove=e=>{if(!drag)return;lon-=(e.clientX-drag[0])*.35;lat=Math.max(-75,Math.min(75,lat+(e.clientY-drag[1])*.25));drag=[e.clientX,e.clientY];redraw();};
  canvas.onpointerup=canvas.onpointercancel=()=>drag=null;
  function reset(){lon=-65;lat=18;redraw();}
  document.getElementById('globe_reset').onclick=reset;
  canvas.onkeydown=e=>{if(e.key==='0'){e.preventDefault();reset();return;}if(!e.key.startsWith('Arrow'))return;e.preventDefault();lon+=e.key==='ArrowLeft'?-10:e.key==='ArrowRight'?10:0;lat=Math.max(-75,Math.min(75,lat+(e.key==='ArrowUp'?10:e.key==='ArrowDown'?-10:0)));redraw();};
  new ResizeObserver(redraw).observe(canvas);
});
