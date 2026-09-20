/* Orthographic globe rendered from bundled Natural Earth / World Atlas outlines.
   No proprietary globe imagery, external globe service or location tracking. */
document.addEventListener('DOMContentLoaded', async () => {
  const canvas=document.getElementById('als-globe');if(!canvas)return;
  const ctx=canvas.getContext('2d'), W=2048,H=1024;
  const texture=document.createElement('canvas');texture.width=W;texture.height=H;
  const t=texture.getContext('2d');t.fillStyle='#236f99';t.fillRect(0,0,W,H);
  const random=n=>{const v=Math.sin(n*127.1+311.7)*43758.5453;return v-Math.floor(v);};
  // A subtle geographic grid gives rotation a readable geographic frame.
  t.strokeStyle='rgba(184,223,238,0.15)';t.lineWidth=1;t.beginPath();
  for(let x=0;x<W;x+=W/12){t.moveTo(x,0);t.lineTo(x,H);}
  for(let y=H/6;y<H;y+=H/6){t.moveTo(0,y);t.lineTo(W,y);}t.stroke();
  const adapters=JSON.parse(canvas.dataset.adapters||'[]');
  const reducedMotion=window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const external=JSON.parse(canvas.dataset.external||'[]');
  let pixels,lon=-65,lat=18,drag=null,pending=false,autorotate=!reducedMotion;
  function setRotation(on){autorotate=on;canvas.dataset.rotating=String(on);}
  setRotation(autorotate);
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
      for(const poly of polys)for(const shift of [-360,0,360]){t.beginPath();poly.forEach(coords=>ring(coords,shift));t.fillStyle=adapters.includes(Number(f.id))?'#e74848':external.includes(Number(f.id))?'#e4c54f':'#c4d5b7';t.fill('evenodd');
        t.strokeStyle='#6c958b';t.lineWidth=.65;t.stroke();}
    }
    // Decorative cloud wisps, generated once in geographic texture space.
    // These rotate with the globe; they are not observed weather or coverage.
    for(let i=0;i<95;i++){
      const x=random(i+401)*W,y=H*(.17+.66*random(i+809));
      const width=22+random(i+199)*85,height=3+random(i+909)*9;
      for(const shift of [-W,0,W]){
        t.save();t.translate(x+shift,y);t.rotate((random(i+57)-.5)*.8);
        t.scale(width,height);
        const cloud=t.createRadialGradient(0,0,0,0,0,1);
        cloud.addColorStop(0,'rgba(239,249,255,.28)');
        cloud.addColorStop(.45,'rgba(223,242,255,.13)');cloud.addColorStop(1,'rgba(223,242,255,0)');
        t.fillStyle=cloud;t.fillRect(-1,-1,2,2);t.restore();
      }
    }
    pixels=t.getImageData(0,0,W,H).data;canvas.dataset.ready='true';draw();
    requestAnimationFrame(spin);
  }catch(e){document.getElementById('globe_status').textContent='Globe unavailable. Open the map to continue.';}
  // Throttled to ~16 fps: the per-pixel software projection in draw() is too
  // costly to re-run at a full 60 fps just for a slow ambient spin.
  let lastSpin=0;
  function spin(ts){if(ts-lastSpin>60){const elapsed=Math.min(ts-lastSpin,150);lastSpin=ts;if(autorotate&&!document.hidden&&canvas.offsetParent){lon+=elapsed*.004;draw();}}requestAnimationFrame(spin);}
  function draw(){
    if(!pixels||!canvas.clientWidth)return;
    const w=Math.min(1100,Math.round(canvas.clientWidth)),h=canvas.clientHeight;
    canvas.width=w;canvas.height=h;ctx.fillStyle='#050b12';ctx.fillRect(0,0,w,h);
    // Two faint, fixed nebula glows behind the starfield for a bit of depth;
    // subtle enough to never compete with the regional coverage outlines.
    const neb1=ctx.createRadialGradient(w*.14,h*.1,0,w*.14,h*.1,w*.4);
    neb1.addColorStop(0,'#3a2f5e3d');neb1.addColorStop(1,'#3a2f5e00');
    ctx.fillStyle=neb1;ctx.fillRect(0,0,w,h);
    const neb2=ctx.createRadialGradient(w*.88,h*.9,0,w*.88,h*.9,w*.45);
    neb2.addColorStop(0,'#1f4a4a3d');neb2.addColorStop(1,'#1f4a4a00');
    ctx.fillStyle=neb2;ctx.fillRect(0,0,w,h);
    const starCount=Math.round(900+600*Math.min(1,Math.max(0,(w-320)/780)));
    for(let i=0;i<starCount;i++){
      const x=random(i+1)*w,y=random(i+1103)*h,tier=i%11;
      ctx.fillStyle=tier===0?'#ffffff':tier<4?'#d0e1ed':'#849fb5';
      const size=tier===0?2.8:1.3;
      ctx.fillRect(x,y,size,size);
      if(i%47===0){ctx.fillStyle='#e4f3ffb3';ctx.fillRect(x-2,y+.5,6,1);ctx.fillRect(x+.5,y-2,1,6);}
    }
    const r=Math.min(w*.43,h*.435),cx=w/2,cy=h/2,phi=lat*Math.PI/180,lambda=lon*Math.PI/180;
    const glow=ctx.createRadialGradient(cx,cy,r*.975,cx,cy,r*1.23);
    glow.addColorStop(0,'rgba(127,224,255,.85)');glow.addColorStop(.2,'rgba(76,191,255,.48)');
    glow.addColorStop(.55,'rgba(48,144,240,.16)');
    glow.addColorStop(1,'rgba(40,121,225,0)');ctx.fillStyle=glow;ctx.fillRect(0,0,w,h);
    const img=ctx.getImageData(0,0,w,h),out=img.data,sky=[106,204,255];
    for(let y=Math.max(0,Math.floor(cy-r));y<Math.min(h,cy+r);y++)for(let x=Math.max(0,Math.floor(cx-r));x<Math.min(w,cx+r);x++){
      const nx=(x-cx)/r,ny=(cy-y)/r,q=nx*nx+ny*ny;if(q>1)continue;
      const z=Math.sqrt(1-q),p=Math.asin(z*Math.sin(phi)+ny*Math.cos(phi)),l=lambda+Math.atan2(nx,z*Math.cos(phi)-ny*Math.sin(phi));
      const tx=Math.floor(((l/(2*Math.PI)+.5)%1+1)%1*W),ty=Math.min(H-1,Math.floor((.5-p/Math.PI)*H));
      const a=(ty*W+tx)*4,b=(y*w+x)*4,light=.55+.45*Math.max(0,z*.88-nx*.3+ny*.22);
      const ocean=pixels[a+2]>pixels[a]*1.5;
      const reflection=ocean?Math.pow(Math.max(0,z*.91-nx*.3+ny*.28),24)*.22:0;
      const atmosphere=Math.pow(1-z,3)*.60;
      for(let k=0;k<3;k++){
        const surface=pixels[a+k]*light;
        const reflected=surface+(240-surface)*reflection;
        out[b+k]=reflected+(sky[k]-reflected)*atmosphere;
      }out[b+3]=255;
    }
    ctx.putImageData(img,0,0);ctx.save();ctx.beginPath();ctx.arc(cx,cy,r,0,Math.PI*2);
    ctx.shadowColor='rgba(99,213,255,.9)';ctx.shadowBlur=r*.035;
    ctx.strokeStyle='rgba(173,237,255,.9)';ctx.lineWidth=2.2;ctx.stroke();ctx.restore();
  }
  function redraw(){if(pending)return;pending=true;requestAnimationFrame(()=>{pending=false;draw();});}
  canvas.onpointerdown=e=>{setRotation(false);drag=[e.clientX,e.clientY];canvas.setPointerCapture(e.pointerId);canvas.focus();};
  canvas.onpointermove=e=>{if(!drag)return;lon-=(e.clientX-drag[0])*.35;lat=Math.max(-75,Math.min(75,lat+(e.clientY-drag[1])*.25));drag=[e.clientX,e.clientY];redraw();};
  canvas.onpointerup=canvas.onpointercancel=()=>drag=null;
  function reset(){lon=-65;lat=18;setRotation(!reducedMotion);redraw();}
  document.getElementById('globe_reset').onclick=reset;
  canvas.onkeydown=e=>{if(e.key==='0'){e.preventDefault();reset();return;}if(!e.key.startsWith('Arrow'))return;e.preventDefault();setRotation(false);lon+=e.key==='ArrowLeft'?-10:e.key==='ArrowRight'?10:0;lat=Math.max(-75,Math.min(75,lat+(e.key==='ArrowUp'?10:e.key==='ArrowDown'?-10:0)));redraw();};
  new ResizeObserver(redraw).observe(canvas);
});
