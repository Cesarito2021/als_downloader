import csv,json,urllib.request,urllib.parse,xml.etree.ElementTree as E,time,hashlib,argparse
from pathlib import Path
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor

def main():
 parser=argparse.ArgumentParser(description='Check every indexed OT asset against its public S3 inventory; read one LAS header per dataset. No cloud is downloaded in full.')
 parser.add_argument('links',type=Path); parser.add_argument('output',type=Path); parser.add_argument('--reuse',type=Path,nargs='*',default=[])
 args=parser.parse_args(); args.output.mkdir(parents=True,exist_ok=True); pages_dir=args.output/'pages';pages_dir.mkdir(exist_ok=True)
 groups=defaultdict(set)
 with args.links.open() as f:
  for r in csv.DictReader(f):groups[r['dataset']].add(r['url'])
 ns={'s':'http://s3.amazonaws.com/doc/2006-03-01/'}; host='https://opentopography.s3.sdsc.edu/pc-bulk/'
 def fetch(url,headers=None,limit=None):
  for attempt in range(3):
   try:
    with urllib.request.urlopen(urllib.request.Request(url,headers=headers or {}),timeout=90) as r:
     return (r.read(limit) if limit else r.read()),r.status
   except Exception:
    if attempt==2:raise
    time.sleep(2**attempt)
 out=[]
 def check(pair):
  name,urls=pair
  expected={urllib.parse.unquote(urllib.parse.urlsplit(u).path).removeprefix('/pc-bulk/'):u for u in urls if u.startswith(host)}
  bad=[u for u in urls if not u.startswith(host)]
  result=None
  for folder in [args.output]+args.reuse:
   summary=folder/f'{name}.json'; sizes=folder/f'{name}-sizes.json'
   if not summary.exists() or not sizes.exists():continue
   r=json.loads(summary.read_text()); objects=json.loads(sizes.read_text())
   if not bad and not r.get('error') and r.get('missing_count')==0 and r.get('sample',{}).get('las_signature') and set(objects)==set(expected):
    result=r; (args.output/f'{name}-sizes.json').write_text(json.dumps(objects));break
  if result is None:
   result={'dataset':name,'links':len(urls)}; found={};pages=0
   try:
    # Enumerate only directories containing indexed tiles. A recursive survey
    # listing may also contain millions of EPT processing nodes unrelated to
    # the original LAS/LAZ tile index.
    for prefix in sorted({k.rsplit('/',1)[0]+'/' for k in expected}):
     token=None;seen=set()
     while True:
      q={'list-type':'2','prefix':prefix,'delimiter':'/','max-keys':'1000'}
      if token:q['continuation-token']=token
      url=host.rstrip('/')+'?'+urllib.parse.urlencode(q)
      cache=pages_dir/(hashlib.sha256(url.encode()).hexdigest()+'.xml')
      if cache.exists():body=cache.read_bytes()
      else:
       body,_=fetch(url);E.fromstring(body);cache.write_bytes(body)
      tree=E.fromstring(body);pages+=1
      for obj in tree.findall('s:Contents',ns):
       key=obj.findtext('s:Key',namespaces=ns);size=int(obj.findtext('s:Size',namespaces=ns))
       if key in expected and size>0:found[key]=size
      if pages%20==0:print(name,'pages',pages,'matched',len(found),'of',len(expected),flush=True)
      if tree.findtext('s:IsTruncated',namespaces=ns)!='true':break
      token=tree.findtext('s:NextContinuationToken',namespaces=ns)
      if not token or token in seen:raise ValueError('Invalid pagination')
      seen.add(token)
    missing=[u for k,u in expected.items() if k not in found]+bad
    result.update(found=len(found),missing_count=len(missing),missing=missing,pages=pages)
    if found:
     key=min(found,key=found.get);url=expected[key];data,status=fetch(url,{'Range':'bytes=0-374'},375)
     result['sample']={'url':url,'size_bytes':found[key],'status':status,'las_signature':data[:4]==b'LASF'}
    (args.output/f'{name}-sizes.json').write_text(json.dumps(found))
   except Exception as e:result['error']=str(e)
  result['url_set_sha256']=hashlib.sha256('\n'.join(sorted(urls)).encode()).hexdigest()
  (args.output/f'{name}.json').write_text(json.dumps(result))
  return result
 with ThreadPoolExecutor(max_workers=2) as pool:
  for n,result in enumerate(pool.map(check,sorted(groups.items())),1):
   out.append(result)
   if n%20==0:print('checked',n,'/',len(groups),flush=True)
 (args.output/'summary.json').write_text(json.dumps(out))
 print('complete',len(out),'errors',sum('error'in r for r in out),'missing',sum(r.get('missing_count',0) for r in out),flush=True)
if __name__=='__main__':main()
