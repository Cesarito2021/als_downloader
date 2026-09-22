"""Offline regression checks: python -m unittest discover -s tools -p test_ot_update.py"""
import copy
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch
import update_ot_catalog as update


def item(identifier='OTLAS.1', name='Survey_A', year='2010'):
    d = dict(identifier={'value': identifier}, name='Survey', url='https://doi.org/example',
             temporalCoverage=year, spatialCoverage={'geometry': 'first'})
    if name:
        d['alternateName'] = name
    return {'Dataset': d}


def catalog(*items):
    return {'Datasets': list(items)}


def state(c):
    rows = update.records(c)
    for x in rows.values():
        x.pop('record')
        x.update(present=True, access_status='out_of_scope', access_reason='Photogrammetry',
                 reviewed_on='2026-09-20')
    return {'records': rows}


class UpdateTests(unittest.TestCase):
    def test_unknown_platform_and_transient_index_errors_block_completion(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            for platform in ('', 'Other', 'Unknown sensor'):
                update.write(root/'dataset-audit-final.json', [dict(dataset='New',platform=platform)])
                with self.assertRaisesRegex(ValueError, 'Metadata unresolved'):
                    update.validate_delta(root)
            update.write(root/'dataset-audit-final.json', [dict(dataset='New',platform='Airborne Lidar',zip_valid=False,index_http_status=503)])
            with self.assertRaisesRegex(ValueError, 'Index check failed'):
                update.validate_delta(root)

    def test_same_records_do_not_repeat_exclusions_or_checks(self):
        c = catalog(item())
        old = state(c)
        plan, _ = update.plan_update(old, c)
        self.assertEqual(plan['changes'], [])
        self.assertEqual(old['records']['OTLAS.1']['access_reason'], 'Photogrammetry')

    def test_new_old_year_is_a_new_site(self):
        c = catalog(item())
        plan, _ = update.plan_update(state(c), catalog(item(), item('OTLAS.2', 'Older', '1998')))
        self.assertEqual([(x['dataset'], x['reason']) for x in plan['changes']], [('Older', 'new')])

    def test_changed_metadata_only_selects_that_site(self):
        c = catalog(item(), item('OTLAS.2', 'B'))
        changed = copy.deepcopy(c)
        changed['Datasets'][1]['Dataset']['citation'] = 'Corrected credit'
        plan, _ = update.plan_update(state(c), changed)
        self.assertEqual([x['dataset'] for x in plan['changes']], ['B'])

    def test_rename_tracks_stable_identity(self):
        c = catalog(item())
        plan, _ = update.plan_update(state(c), catalog(item(name='Renamed')))
        self.assertEqual(plan['changes'][0]['previous_dataset'], 'Survey_A')

    def test_missing_retained_and_reappearance_rechecked(self):
        c = catalog(item(), item('OTLAS.2', 'B'))
        old = state(c)
        plan, _ = update.plan_update(old, catalog(item()))
        self.assertEqual(plan['missing'][0]['id'], 'OTLAS.2')
        self.assertIn('OTLAS.2', old['records'])
        old['records']['OTLAS.2']['present'] = False
        plan, _ = update.plan_update(old, c)
        self.assertEqual(plan['changes'][0]['reason'], 'reappeared')

    def test_targeted_availability_check(self):
        c = catalog(item(), item('OTLAS.2', 'B'))
        plan, _ = update.plan_update(state(c), c, ['B'])
        self.assertEqual([x['dataset'] for x in plan['changes']], ['B'])
        with self.assertRaises(ValueError):
            update.plan_update(state(c), c, ['typo'])

    def test_otds_geometry_variants_order_does_not_change_fingerprint(self):
        a = item('OTDS.1', None)
        b = copy.deepcopy(a)
        b['Dataset']['spatialCoverage']['geometry'] = 'bbox'
        self.assertEqual(update.records(catalog(a,b)), update.records(catalog(b,a,a)))
        with self.assertRaises(ValueError):
            update.records(catalog(item(), item()))

    def test_empty_catalog_and_reassigned_names_fail(self):
        with self.assertRaises(ValueError):
            update.records(catalog())
        with self.assertRaises(ValueError):
            update.plan_update(state(catalog(item())), catalog(item('OTLAS.2')))
        with self.assertRaises(ValueError):
            update.records(catalog(item(name='../escape')))

    def test_corrupt_baseline_rejected(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            update.write(root/update.STATE, {'schema_version':1, 'files':{}})
            for name in update.FILES:
                (root/name).write_text('corrupt')
            with self.assertRaises(ValueError):
                update.validate_baseline(root)

    def test_failure_never_marks_complete_or_edits_baseline(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            baseline = root/'baseline'
            baseline.mkdir()
            sentinel = baseline/'sentinel'
            sentinel.write_text('original')
            with patch.object(update, 'validate_baseline', return_value=state(catalog(item()))), \
                 patch.object(update.subprocess, 'run', side_effect=RuntimeError('network failure')):
                with self.assertRaises(RuntimeError):
                    update.run_update(baseline, root/'candidate', catalog(item(name='Renamed')), 'Rscript')
            self.assertFalse((root/'candidate'/'complete.json').exists())
            self.assertEqual(sentinel.read_text(), 'original')
            self.assertEqual(list(baseline.iterdir()), [sentinel])

    def test_unchanged_catalog_needs_no_r_or_tile_requests(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            for name in (*update.FILES, update.STATE):
                (root/name).write_text('unchanged fixture')
            with patch.object(update, 'validate_baseline', return_value=state(catalog(item()))), \
                 patch.object(update.subprocess, 'run') as run:
                update.run_update(root, root/'candidate', catalog(item()), 'Rscript')
                run.assert_not_called()
                self.assertTrue((root/'candidate'/'complete.json').exists())
                self.assertEqual((root/'candidate'/'catalog'/update.FILES[0]).read_text(),'unchanged fixture')

    def test_missing_requires_explicit_review_and_new_output_directory(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            with patch.object(update, 'validate_baseline', return_value=state(catalog(item(),item('OTLAS.2','B')))), \
                 patch.object(update.subprocess, 'run') as run:
                with self.assertRaisesRegex(ValueError, 'omits known IDs'):
                    update.run_update(root, root/'candidate', catalog(item()), 'Rscript')
                run.assert_not_called()
                self.assertFalse((root/'candidate'/'complete.json').exists())
                with self.assertRaises(FileExistsError):
                    update.run_update(root, root/'candidate', catalog(item()), 'Rscript')


if __name__ == '__main__':
    unittest.main()
