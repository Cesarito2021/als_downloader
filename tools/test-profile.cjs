// Offline geometry checks for the visual profile, with no point-cloud downloads.
const fs=require('fs'),vm=require('vm'),assert=require('assert'),path=require('path');
const context={window:{}};vm.createContext(context);
vm.runInContext(fs.readFileSync(path.join(__dirname,'../inst/app/www/profile.js'),'utf8'),context);
const section=(...args)=>JSON.parse(JSON.stringify(context.window.ALSProfileSection(...args)));
const p=[[0,0,100],[5,0,105],[5,1,106],[5,1.1,107],[-1,0,108],[11,0,109]],groups=[0,1,0,1,0,1];
assert.deepStrictEqual(section(p,groups,[0,0],[10,0],2),[[0,100,0],[5,105,1],[5,106,0]]);
assert.deepStrictEqual(section(p,groups,[10,0],[0,0],2),[[10,100,0],[5,105,1],[5,106,0]]);
const d=section([[2,2,10],[4,4,20],[2,3,30]],[0,1,0],[0,0],[6,6],.2);
assert.equal(d.length,2);assert(Math.abs(d[1][0]-Math.sqrt(32))<1e-10);
assert.deepStrictEqual(section(p,groups,[0,0],[0,0],2),[]);
assert.deepStrictEqual(section(p,groups,[20,20],[30,20],2),[]);
console.log('PASS: directional distance, strip/endpoint clipping, original elevations/groups and empty sections.');
