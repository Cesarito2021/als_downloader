"""Summarize audited OT storage without counting a shared file twice."""
import argparse
import csv
import hashlib
import json
from pathlib import Path


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('audit',type=Path)
    p.add_argument('objects',type=Path)
    p.add_argument('output',type=Path)
    args=p.parse_args()
    with args.audit.open(encoding='utf-8-sig',newline='') as f:
        rows=[r for r in csv.DictReader(f) if r['access_status']=='ready']
    unique={}; summary=[]
    for row in rows:
        name=row['dataset']
        sizes=json.loads((args.objects/(name+'-sizes.json')).read_text())
        proof=json.loads((args.objects/(name+'.json')).read_text())
        if proof['url_set_sha256']!=row['url_set_sha256'] or len(sizes)!=int(row['verified_objects']):
            raise ValueError('Object audit does not match catalogue: '+name)
        for key,size in sizes.items():
            if not isinstance(size,int) or size<=0 or (key in unique and unique[key]!=size):
                raise ValueError('Invalid or conflicting object size: '+name)
            unique[key]=size
        summary.append(dict(dataset=name,title=row['title'],doi=row['doi'],
            tile_records=int(row['tile_count']),distinct_objects=len(sizes),
            bytes=sum(sizes.values()),reviewed_on=row['reviewed_on']))
    result=dict(provider='OpenTopography',scope='Verified hosted airborne-LiDAR collections only; excludes federated 3DEP and external-only collections',
        reviewed_on=sorted({r['reviewed_on'] for r in rows}),collections=len(rows),
        tile_footprint_records=sum(int(r['tile_count']) for r in rows),
        unique_file_objects=len(unique),bytes=sum(unique.values()),
        terabytes_decimal=sum(unique.values())/10**12,tebibytes=sum(unique.values())/2**40,
        method='Sum positive object sizes from the verified public inventory, deduplicated by object key across collections. No point-cloud files are hosted by this package. Geographic overlap is not deduplicated; distinct files can cover the same ground.',
        audit_sha256=hashlib.sha256(args.audit.read_bytes()).hexdigest())
    args.output.mkdir(parents=True,exist_ok=True)
    (args.output/'coverage-statistics.json').write_text(json.dumps(result,indent=2)+'\n')
    with (args.output/'opentopography-collection-statistics.csv').open('w',encoding='utf-8',newline='') as f:
        writer=csv.DictWriter(f,fieldnames=list(summary[0]));writer.writeheader();writer.writerows(summary)
    print(json.dumps(result,indent=2))


if __name__=='__main__':main()
