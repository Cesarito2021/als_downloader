// Exercise the actual drawing handlers without a browser or network.
const fs=require('fs'),vm=require('vm'),assert=require('assert'),path=require('path');
const labels=[],handlers={},colours={};let ready;
const canvas=id=>({clientWidth:600,clientHeight:400,getContext:()=>new Proxy({
  fillText:text=>{if(id==='als-compare-density')labels.push(String(text));},
  fillRect:function(x,y,w,h){if(w===1.8&&h===1.8)(colours[id]||=[]).push(this.fillStyle);},
  createLinearGradient:()=>({addColorStop(){}})
},{get:(object,key)=>object[key]||(()=>{})})});
const elements=Object.fromEntries(['als-compare-cloud-a','als-compare-cloud-b','als-compare-density'].map(id=>[id,canvas(id)]));
const context={window:{},devicePixelRatio:1,ResizeObserver:class{observe(){} disconnect(){}},
  matchMedia:()=>({addEventListener(){}}),Shiny:{addCustomMessageHandler:(key,handler)=>handlers[key]=handler},
  document:{getElementById:id=>elements[id]||null,querySelector:()=>null,addEventListener:(name,handler)=>{ready=handler;}}};
vm.createContext(context);vm.runInContext(fs.readFileSync(path.join(__dirname,'../inst/app/www/preview.js'),'utf8'),context);ready();
handlers['als-points']({target:'als-compare-cloud',points:[[0,0,5],[1,1,10]],groups:[0,1],origin:[0,0,1000]});
assert(labels.includes('A: n=1, mean 1005.00 m'));
assert(labels.includes('B: n=1, mean 1010.00 m'));
assert(labels.includes('1005.0 m'));assert(labels.includes('1010.0 m'));
labels.length=0;handlers['als-view']({target:'als-compare-cloud',showA:false});
assert(labels.includes('A: n=0 (hidden or none loaded)'));
assert(labels.includes('B: n=1, mean 1010.00 m'));
handlers['als-view']({target:'als-compare-cloud',showB:false});
assert(labels.some(x=>x.startsWith('Both clouds are hidden')));
console.log('PASS: absolute elevation distributions, mean values and visibility.');
handlers['als-view']({target:'als-compare-cloud',showA:true,showB:true,cloudMode:'shared',sharedPalette:'Greens'});
colours['als-compare-cloud-a']=[];colours['als-compare-cloud-b']=[];
handlers['als-points']({target:'als-compare-cloud',points:[[0,0,5],[1,1,5]],groups:[0,1],origin:[0,0,1000]});
assert.equal(colours['als-compare-cloud-a'][0],colours['als-compare-cloud-b'][0]);
colours['als-compare-cloud-a']=[];colours['als-compare-cloud-b']=[];
handlers['als-view']({target:'als-compare-cloud',cloudMode:'campaign'});
assert.notEqual(colours['als-compare-cloud-a'][0],colours['als-compare-cloud-b'][0]);
console.log('PASS: equal source elevations share colours; campaign mode preserves distinct A/B colours.');
