"""Incremental OT maintenance. Stage immutable snapshots; never edit the baseline.

python tools/update_ot_catalog.py --baseline inst/extdata --output SNAPSHOT --plan-only
python tools/update_ot_catalog.py --baseline inst/extdata --output SNAPSHOT --rscript Rscript
Use --recheck ID for a targeted availability / metadata check independent of age.
"""
import argparse
import csv
import hashlib
import json
import re
import shutil
import subprocess
import sys
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

FILES = ('opentopography-registry.rds', 'opentopography-access-audit.csv',
         'opentopography-verification.json')
STATE = 'opentopography-update-state.json'
TOOLS = Path(__file__).resolve().parent
QUERY = dict(productFormat='PointCloud', minx=-180, miny=-90, maxx=180, maxy=90,
             detail='true', outputFormat='json', include_federated='false')
ENDPOINT = 'https://portal.opentopography.org/API/otCatalog?'+urllib.parse.urlencode(QUERY)


def read(path):
    return json.loads(Path(path).read_text(encoding='utf-8-sig'))


def write(path, value):
    Path(path).write_text(json.dumps(value, indent=2, ensure_ascii=False)+'\n', encoding='utf-8')


def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for block in iter(lambda: f.read(1024*1024), b''):
            h.update(block)
    return h.hexdigest()


def records(catalog):
    if not isinstance(catalog.get('Datasets'), list) or not catalog['Datasets']:
        raise ValueError('Empty or malformed catalogue; keep the current snapshot')
    result, names, variants = {}, set(), {}
    for wrapped in catalog['Datasets']:
        d = wrapped['Dataset']
        identifier = d['identifier']['value']
        if not isinstance(identifier, str) or not identifier:
            raise ValueError('Missing stable collection ID')
        canonical = json.dumps(d, sort_keys=True, separators=(',', ':'), ensure_ascii=False)
        if identifier in result:
            previous = result[identifier]['record']['Dataset']
            # OTDS publishes polygon and bounding-box variants for the same ID.
            # Preserve all variants in its fingerprint; never merge hosted IDs.
            if d.get('alternateName') or previous.get('alternateName') or any(d.get(k) != previous.get(k) for k in ('name','url')):
                raise ValueError('Conflicting duplicate stable collection ID')
            variants[identifier].add(canonical)
            continue
        name = d.get('alternateName') or identifier
        if name in names:
            raise ValueError('Duplicate dataset name')
        if d.get('alternateName') and not re.fullmatch(r'[A-Za-z0-9_-]+', name):
            raise ValueError('Unsafe dataset name')
        names.add(name)
        variants[identifier] = {canonical}
        result[identifier] = dict(dataset=name, record=wrapped)
    for identifier, item in result.items():
        canonical_variants = sorted(variants[identifier])
        item['fingerprint'] = hashlib.sha256('\n'.join(canonical_variants).encode()).hexdigest()
        item['record'] = {'Dataset': json.loads(canonical_variants[0])}
    return result


def bootstrap(catalog_path, baseline, destination):
    """Bind the original audit to its exact catalogue; no network or reanalysis."""
    proof = read(baseline/'opentopography-verification.json')
    if sha(catalog_path) != proof['catalog_sha256']:
        raise ValueError('Bootstrap catalogue does not match the original audit SHA-256')
    with (baseline/'opentopography-access-audit.csv').open(encoding='utf-8-sig', newline='') as f:
        audit = {r['dataset']: r for r in csv.DictReader(f)}
    items = records(read(catalog_path))
    for item in items.values():
        row = audit[item['dataset']]
        item.pop('record')
        item.update(reviewed_on=row['reviewed_on'], access_status=row['access_status'],
                    access_reason=row['access_reason'], present=True)
    write(destination, dict(schema_version=1, records=items,
          files={name: sha(baseline/name) for name in FILES}))


def validate_baseline(baseline):
    state = read(baseline/STATE)
    if state.get('schema_version') != 1:
        raise ValueError('Unsupported update state')
    for name in FILES:
        if sha(baseline/name) != state['files'].get(name):
            raise ValueError('Baseline changed since verification: '+name)
    return state


def plan_update(state, catalog, recheck=()):
    current = records(catalog)
    old = state['records']
    requested = set(recheck)
    known = set(current) | {x['dataset'] for x in current.values()}
    if requested-known:
        raise ValueError('Unknown --recheck ID or dataset: '+', '.join(sorted(requested-known)))
    changes, unchanged, missing = [], [], []
    for identifier, item in current.items():
        previous = old.get(identifier)
        reason = ('new' if previous is None else
                  'reappeared' if not previous.get('present', True) else
                  'changed' if item['fingerprint'] != previous['fingerprint'] else
                  'requested' if identifier in requested or item['dataset'] in requested else None)
        if reason:
            changes.append(dict(id=identifier, dataset=item['dataset'], reason=reason,
                                previous_dataset=previous['dataset'] if previous else None))
        else:
            unchanged.append(identifier)
    for identifier, previous in old.items():
        if identifier not in current:
            missing.append(dict(id=identifier, dataset=previous['dataset']))
    # Never confuse a rename with another collection taking its old name.
    owners = {v['dataset']: k for k, v in old.items()}
    for identifier, item in current.items():
        if item['dataset'] in owners and owners[item['dataset']] != identifier:
            raise ValueError('Dataset name was reassigned to another ID; manual review required')
    return dict(changes=changes, unchanged=unchanged, missing=missing), current


def validate_delta(root):
    for x in read(root/'dataset-audit-final.json'):
        known_platforms = ('Airborne Lidar', 'Structure from Motion / Photogrammetry',
                           'Terrestrial Laser Scanner')
        if x.get('metadata_error') or x.get('platform') not in known_platforms:
            raise ValueError('Metadata unresolved: '+x['dataset'])
        if x['platform'] == 'Airborne Lidar' and not x.get('zip_valid'):
            if x.get('index_http_status') != 404:
                raise ValueError('Index check failed (not a confirmed missing index): '+x['dataset'])
    with (root/'index-summary-current.csv').open(newline='', encoding='utf-8-sig') as f:
        for row in csv.DictReader(f):
            if row['error'] or row['missing'] != '0' or row['crs'] != 'FALSE' or int(row['tiles']) == 0:
                raise ValueError('Invalid tile index: '+row['dataset'])
    for x in read(root/'objects-verified'/'summary.json'):
        if x.get('error') or x.get('missing_count') != 0 or not x.get('sample', {}).get('las_signature'):
            raise ValueError('Tile access check failed: '+x['dataset'])


def run_update(baseline, output, catalog, rscript, recheck=(), plan_only=False,
               allow_missing=False):
    state = validate_baseline(baseline)
    plan, current = plan_update(state, catalog, recheck)
    # mkdir without exist_ok prevents reuse of stale object inventories.
    output.mkdir(parents=True, exist_ok=False)
    plan['checked_at'] = datetime.now(timezone.utc).isoformat()
    plan['baseline'] = str(baseline.resolve())
    write(output/'update-plan.json', plan)
    write(output/'pointcloud-catalog.json', catalog)
    print(json.dumps({k: len(plan[k]) for k in ('changes', 'unchanged', 'missing')}), flush=True)
    if plan_only:
        return
    if plan['missing'] and not allow_missing:
        raise ValueError('Catalogue omits known IDs. Review update-plan.json; retry in a new output directory with --allow-missing to retain them as external-only')
    delta = output/'audit'
    delta.mkdir()
    selection = {'Datasets': [current[x['id']]['record'] for x in plan['changes']]}
    write(output/'selected-catalog.json', selection)
    candidate = output/'catalog'
    candidate.mkdir()
    if not plan['changes'] and not plan['missing']:
        for name in (*FILES, STATE):
            shutil.copy2(baseline/name, candidate/name)
        validate_baseline(candidate)
        write(output/'complete.json', dict(completed_at=datetime.now(timezone.utc).isoformat(),
              catalog='catalog', files={name: sha(candidate/name) for name in (*FILES, STATE)}))
        print('No changes: reused the verified catalogue without R or tile requests.', flush=True)
        return
    def run(script, *args):
        interpreter = rscript if script.endswith('.R') else sys.executable
        subprocess.run([str(interpreter), str(TOOLS/script), *map(str, args)], check=True)
    run('materialize_ot_baseline.R', baseline, output/'full-baseline')
    if (output/'full-baseline').exists():
        baseline = output/'full-baseline'
        validate_baseline(baseline)
    if plan['changes']:
        run('refresh_ot_metadata.py', delta, '--catalog', output/'selected-catalog.json')
        run('scan_ot_indexes.R', delta)
        run('audit_ot_objects.py', delta/'tile-links-current.csv', delta/'objects-verified')
        validate_delta(delta)
        run('build_ot_registry.R', delta, output/'delta-catalog', baseline)
    run('merge_ot_update.R', baseline, output)
    with (candidate/'opentopography-access-audit.csv').open(encoding='utf-8-sig', newline='') as f:
        audit = {r['dataset']: r for r in csv.DictReader(f)}
    merged = dict(state['records'])
    for identifier, item in current.items():
        row = audit[item['dataset']]
        merged[identifier] = dict(dataset=item['dataset'], fingerprint=item['fingerprint'],
            reviewed_on=row['reviewed_on'], access_status=row['access_status'],
            access_reason=row['access_reason'], present=True)
    for x in plan['missing']:
        merged[x['id']] = dict(merged[x['id']], present=False)
    write(candidate/STATE, dict(schema_version=1, records=merged,
          files={name: sha(candidate/name) for name in FILES}))
    validate_baseline(candidate)
    # Completion marker is written last. Consumers must ignore unfinished folders.
    write(output/'complete.json', dict(completed_at=datetime.now(timezone.utc).isoformat(),
          catalog='catalog', files={name: sha(candidate/name) for name in (*FILES, STATE)}))
    print('Verified snapshot ready:', candidate, flush=True)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--baseline', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--catalog', type=Path, help='Offline catalogue for reproducible verification')
    p.add_argument('--rscript', default='Rscript')
    p.add_argument('--recheck', action='append', default=[])
    p.add_argument('--plan-only', action='store_true')
    p.add_argument('--allow-missing', action='store_true')
    p.add_argument('--initialize-state', action='store_true',
                   help='Bind a completed full audit to its exact original --catalog; output is a new state JSON file')
    args = p.parse_args()
    if args.initialize_state:
        if not args.catalog or args.output.exists():
            p.error('--initialize-state requires --catalog and a new output filename')
        bootstrap(args.catalog, args.baseline, args.output)
        return
    if args.catalog:
        catalog = read(args.catalog)
    else:
        with urllib.request.urlopen(ENDPOINT, timeout=90) as response:
            catalog = json.load(response)
    run_update(args.baseline, args.output, catalog, args.rscript, args.recheck,
               args.plan_only, args.allow_missing)


if __name__ == '__main__':
    main()
