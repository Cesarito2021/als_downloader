"""Explicit bounded access check for the Italian ALS record; never downloads the full file."""
import json,urllib.request,struct,hashlib,math
from pathlib import Path
from datetime import datetime,timezone
root=Path(__file__).resolve().parents[1]
with urllib.request.urlopen(urllib.request.Request('https://zenodo.org/api/records/3633629', headers={'User-Agent':'ALS-source-review/1.0'}), timeout=40) as response:
 data=json.load(response)
asset=next(f for f in data['files'] if f['key']=='merged.las');url=asset['links']['self']
def fetch(start,end):
 request=urllib.request.Request(url,headers={'User-Agent':'ALS-source-review/1.0','Range':f'bytes={start}-{end}'})
 with urllib.request.urlopen(request,timeout=45) as r:
  if r.status!=206:raise RuntimeError('Server did not honor bounded range; full multi-GB download intentionally not started')
  content=r.read(end-start+2)
  if len(content)!=end-start+1:raise RuntimeError('Unexpected range length')
  content_range=r.headers.get('Content-Range','')
  if not content_range.startswith(f'bytes {start}-{end}/'):raise RuntimeError('Incorrect Content-Range')
  return content,content_range
raw,content_range=fetch(0,65535)
assert raw[:4]==b'LASF'
offset=struct.unpack_from('<I',raw,96)[0];fmt=raw[104];length=struct.unpack_from('<H',raw,105)[0]
assert not fmt&128,'Compressed LAS requires a LAZ decoder'
count=struct.unpack_from('<I',raw,107)[0]
if raw[25]>=4 and len(raw)>=255:count=struct.unpack_from('<Q',raw,247)[0] or count
scales=struct.unpack_from('<3d',raw,131);origins=struct.unpack_from('<3d',raw,155)
sample,points_range=fetch(offset,offset+1024*length-1)
xyz=[tuple(v*s+o for v,s,o in zip(struct.unpack_from('<3i',sample,i*length),scales,origins)) for i in range(1024)]
assert all(math.isfinite(v) for p in xyz for v in p)
summary={'checked_at_utc':datetime.now(timezone.utc).isoformat(),'record_url':'https://zenodo.org/records/3633629','doi':data['metadata']['doi'],'creator':'Nicola Puletti (CREA)','license':data['metadata']['license']['id'],'file':asset['key'],'url':url,'published_file_bytes':asset['size'],'published_md5':asset['checksum'],'full_file_checksum_verified':False,'http_status':206,'header_content_range':content_range,'points_content_range':points_range,'bytes_read':len(raw)+len(sample),'las_version':f'{raw[24]}.{raw[25]}','point_format':fmt,'point_record_length':length,'header_point_count':count,'decoded_sample_points':1024,'sample_xyz_min':[min(p[j] for p in xyz) for j in range(3)],'sample_xyz_max':[max(p[j] for p in xyz) for j in range(3)],'header_sha256':hashlib.sha256(raw).hexdigest(),'sample_sha256':hashlib.sha256(sample).hexdigest(),'limitation':'Two bounded HTTP ranges, not a complete tile download or full-file validation. XYZ decoded from the first 1024 uncompressed LAS point records. CRS not assumed from GPS filename.'}
(root/'docs/italy-sila-access.json').write_text(json.dumps(summary,indent=2),encoding='utf8')
print(json.dumps(summary,indent=2))
