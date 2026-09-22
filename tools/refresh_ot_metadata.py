"""Refresh public hosted OT metadata and index ZIPs (no point-cloud downloads).
Usage: python tools/refresh_ot_metadata.py AUDIT_DIR
Run in a fresh directory for a new audit. No account or API key is used.
"""
import argparse,hashlib,html,json,re,urllib.parse,urllib.request,zipfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

def main():
 parser=argparse.ArgumentParser();parser.add_argument('output',type=Path)
 parser.add_argument('--catalog',type=Path,help='Use a prepared catalogue containing only selected records')
 args=parser.parse_args()
 root=args.output;root.mkdir(parents=True,exist_ok=True)
 for folder in ['pages','indexes']:(root/folder).mkdir(exist_ok=True)
 query={'productFormat':'PointCloud','minx':-180,'miny':-90,'maxx':180,'maxy':90,'detail':'true','outputFormat':'json','include_federated':'false'}
 if args.catalog:body=args.catalog.read_bytes()
 else:
  with urllib.request.urlopen('https://portal.opentopography.org/API/otCatalog?'+urllib.parse.urlencode(query),timeout=90) as r:body=r.read()
 catalog=json.loads(body);(root/'pointcloud-catalog.json').write_bytes(body)
 def plain(s):return re.sub(r'\s+',' ',html.unescape(re.sub('<[^>]+>',' ',s))).strip()
 def read_page(url):
  with urllib.request.urlopen(url,timeout=60) as r:return r.url,r.read().decode(r.headers.get_content_charset() or 'utf8',errors='replace')
 def check(d):
  name=d['alternateName'];x={'dataset':name,'title':d['name'],'id':d['identifier']['value'],'doi':d['url'],'temporal':d.get('temporalCoverage',''),'citation':d.get('citation','')}
  if not re.fullmatch(r'[A-Za-z0-9_-]+',name):raise ValueError('Unsafe dataset name')
  x['index_url']='https://opentopography.s3.sdsc.edu/pc-bulk/'+name+'/'+name+'_TileIndex.zip'
  x['info_url']='https://portal.opentopography.org/datasetMetadata?otCollectionID='+x['id'].replace('OTLAS.','OT.')
  x.update(platform='',license_url='',license_source='',zip_valid=False)
  try:
   url,s=read_page(x['info_url'])
   if not re.search(r'<strong>Collection Platform</strong>\s*:\s*[^<\r\n]+',s,re.I):url,s=read_page(x['doi'])
   x['info_url']=url;(root/'pages'/f'{name}.html').write_text(s,encoding='utf8')
   def field(label):
    m=re.search(r'<strong>'+re.escape(label)+r'</strong>\s*:\s*(.*?)(?=<strong>|</div>)',s,re.S|re.I)
    return m.group(1).strip() if m else ''
   m=re.search(r'<strong>Collection Platform</strong>\s*:\s*([^<\r\n]+)',s,re.I)
   x['platform']=html.unescape(m.group(1)).strip() if m else ''
   x['acknowledgement']=plain(field('Dataset Acknowledgement'))
   m=re.search(r'href=[\"\x27]([^\"\x27]+)',field('Use License'))
   x['license_url']=html.unescape(m.group(1)) if m else '';x['license_source']='Dataset use license'
   if not x['license_url']:
    m=re.search(r'https?://creativecommons.org/licenses/[^\s<\")]+',x['acknowledgement'])
    if m:x['license_url']=m.group(0).rstrip('.');x['license_source']='Dataset acknowledgement'
    elif 'Attribution 3.0 New Zealand' in x['acknowledgement']:
     x['license_url']='https://creativecommons.org/licenses/by/3.0/nz/';x['license_source']='Dataset acknowledgement'
    else:x['license_url']='https://opentopography.org/usageterms';x['license_source']='OpenTopography Data and Content terms; no dataset-specific license supplied'
   if x['citation'].startswith('N/A') or not x['citation']:x['citation']=x['title']+'. Distributed by OpenTopography. '+x['doi']
   if x['acknowledgement']:x['citation']+=' Dataset acknowledgement: '+x['acknowledgement']
  except Exception as e:x['metadata_error']=str(e)
  # Classification precedes the expensive tile and object work.
  if x.get('metadata_error') or x['platform']!='Airborne Lidar':return x
  try:
   with urllib.request.urlopen(x['index_url'],timeout=90) as r:data=r.read(150*1024*1024+1)
   if len(data)>150*1024*1024:raise ValueError('Index exceeds size limit')
   path=root/'indexes'/f'{name}_TileIndex.zip';path.write_bytes(data)
   if not zipfile.is_zipfile(path):raise ValueError('Response is not a ZIP')
   x.update(zip_valid=True,index_sha256=hashlib.sha256(data).hexdigest(),index_bytes=len(data))
  except Exception as e:
   x['error']=str(e)
   x['index_http_status']=getattr(e,'code',None)
   try:
    url,_=read_page('https://portal.opentopography.org/opentopoS3?opentopoID='+x['id']+'&tiledIndex=1')
    x['index_portal_url']=url
   except Exception as e:x['index_portal_error']=str(e)
  return x
 rows=[]
 with ThreadPoolExecutor(max_workers=2) as pool:
  for x in pool.map(check,[r['Dataset'] for r in catalog['Datasets'] if r['Dataset'].get('alternateName')]):
   rows.append(x)
   if len(rows)%25==0:print('metadata',len(rows),flush=True)
 (root/'dataset-audit-final.json').write_text(json.dumps(rows),encoding='utf8')
 print('Complete:',len(rows),'hosted records;',sum(x['zip_valid'] for x in rows),'index ZIPs')
if __name__=='__main__':main()
