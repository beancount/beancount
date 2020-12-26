__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import functools
import logging
import importlib
import unittest
import tempfile
import textwrap
import os
from unittest import mock
from os import path

from beancount import loader
from beancount.parser import parser
from beancount.utils import test_utils
from beancount.utils import encryption_test


TEST_INPUT = """

2014-01-01 open Assets:MyBank:Checking   USD
2014-01-01 open Expenses:Restaurant   USD

2014-02-22 * "Something happened."
  Assets:MyBank:Checking       100.00 USD
  Expenses:Restaurant         -100.00 USD

2015-01-01 close Assets:MyBank:Checking
2015-01-01 close Expenses:Restaurant

"""


def get_failing_plugin_module(exc_type):
    """Return a failing plugin module. For testing."""
    class PluginModule: pass
    def fail(*args):
        raise exc_type()
    PluginModule.__plugins__ = (fail,)
    return PluginModule


real_import_module = importlib.import_module


def mock_import_module(exc_type, plugin_name):
    if plugin_name == "failing":
        return get_failing_plugin_module(exc_type)
    else:
        return real_import_module(plugin_name)


class TestLoader(unittest.TestCase):

    def test_import_exception(self):
        # Test an invalid plugin name.
        entries, errors, options_map = parser.parse_string(
            'plugin "invalid.module.name"\n\n' + TEST_INPUT)
        trans_entries, trans_errors = loader.run_transformations(
            entries, errors, options_map, None)
        self.assertEqual(1, len(trans_errors))
        self.assertRegex(trans_errors[0].message, "ModuleNotFoundError")

    @mock.patch('importlib.import_module', side_effect=ValueError)
    def test_import_other_exception(self, _):
        # Test another exception occurring during import.
        entries, errors, options_map = parser.parse_string(
            'plugin "doesnt_matter"\n\n' + TEST_INPUT)
        with self.assertRaises(ValueError):
            loader.run_transformations(entries, errors, options_map, None)

    @mock.patch('importlib.import_module',
                functools.partial(mock_import_module, ValueError))
    def test_run_transformation_exception(self):
        # Test another exception occurring during import.
        entries, errors, options_map = parser.parse_string(
            'plugin "failing"\n\n' + TEST_INPUT)
        loader.run_transformations(entries, errors, options_map, None)
        trans_entries, trans_errors = loader.run_transformations(
            entries, errors, options_map, None)
        self.assertEqual(1, len(trans_errors))
        self.assertRegex(trans_errors[0].message, "ValueError")

    @mock.patch('importlib.import_module',
                functools.partial(mock_import_module, SystemExit))
    def test_run_transformation_systemexit(self):
        # Test another exception occurring during import.
        entries, errors, options_map = parser.parse_string(
            'plugin "failing"\n\n' + TEST_INPUT)
        with self.assertRaises(SystemExit):
            loader.run_transformations(entries, errors, options_map, None)

    def test_run_transformations(self):
        # Test success case.
        entries, errors, options_map = parser.parse_string(TEST_INPUT)
        trans_entries, trans_errors = loader.run_transformations(
            entries, errors, options_map, None)
        self.assertEqual(0, len(trans_errors))

    def test_load(self):
        with test_utils.capture():
            with tempfile.NamedTemporaryFile('w') as tmpfile:
                tmpfile.write(TEST_INPUT)
                tmpfile.flush()
                entries, errors, options_map = loader.load_file(tmpfile.name)
                self.assertTrue(isinstance(entries, list))
                self.assertTrue(isinstance(errors, list))
                self.assertTrue(isinstance(options_map, dict))

                entries, errors, options_map = loader.load_file(tmpfile.name,
                                                                log_timings=logging.info)
                self.assertTrue(isinstance(entries, list))
                self.assertTrue(isinstance(errors, list))
                self.assertTrue(isinstance(options_map, dict))

    def test_load_string(self):
        with test_utils.capture():
            entries, errors, options_map = loader.load_string(TEST_INPUT)
            self.assertTrue(isinstance(entries, list))
            self.assertTrue(isinstance(errors, list))
            self.assertTrue(isinstance(options_map, dict))

            entries, errors, options_map = loader.load_string(TEST_INPUT,
                                                              log_timings=logging.info)
            self.assertTrue(isinstance(entries, list))
            self.assertTrue(isinstance(errors, list))
            self.assertTrue(isinstance(options_map, dict))

    def test_load_nonexist(self):
        entries, errors, options_map = loader.load_file('/some/bullshit/filename.beancount')
        self.assertEqual([], entries)
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'does not exist')

    @mock.patch.dict(loader.RENAMED_MODULES,
                     {"beancount.ops.auto_accounts": "beancount.plugins.auto_accounts"},
                     clear=True)
    @mock.patch('warnings.warn')
    def test_renamed_plugin_warnings(self, warn):
        with test_utils.capture('stderr'):
            entries, errors, options_map = loader.load_string("""
              plugin "beancount.ops.auto_accounts"
            """, dedent=True)
        self.assertTrue(warn.called)
        self.assertFalse(errors)


class TestLoadDoc(unittest.TestCase):

    def test_load_doc(self):
        def test_function(self_, entries, errors, options_map):
            self.assertTrue(isinstance(entries, list))
            self.assertTrue(isinstance(errors, list))
            self.assertTrue(isinstance(options_map, dict))

        test_function.__doc__ = TEST_INPUT
        test_function = loader.load_doc(test_function)
        test_function(self)

    # pylint: disable=empty-docstring
    @loader.load_doc()
    def test_load_doc_empty(self, entries, errors, options_map):
        """
        """
        self.assertTrue(isinstance(entries, list))
        self.assertTrue(isinstance(errors, list))
        self.assertFalse(errors)
        self.assertTrue(isinstance(options_map, dict))

    @loader.load_doc(expect_errors=True)
    def test_load_doc_plugin(self, entries, errors, options_map):
        """
        plugin "beancount.does.not.exist"
        """
        self.assertTrue(isinstance(entries, list))
        self.assertTrue(isinstance(options_map, dict))
        self.assertTrue([loader.LoadError], list(map(type, errors)))

    def test_load_doc_plugin_auto_pythonpath(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            ledger_fn = path.join(tmpdir, 'my.beancount')
            with open(ledger_fn, 'w') as ledger_file:
                ledger_file.write('option "insert_pythonpath" "TRUE"\n')
                ledger_file.write('plugin "localplugin"\n')

            plugin_fn = path.join(tmpdir, 'localplugin.py')
            with open(plugin_fn, 'w') as plugin_file:
                plugin_file.write(textwrap.dedent("""\
                  __plugins__ = ()
                """))
            entries, errors, options_map = loader.load_file(ledger_fn)
            self.assertTrue(isinstance(entries, list))
            self.assertTrue(isinstance(errors, list))
            self.assertTrue(isinstance(options_map, dict))
            self.assertFalse(errors)


class TestLoadIncludes(unittest.TestCase):

    def test_load_file_no_includes(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  2014-01-01 open Assets:Apples
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'apples.beancount'))
            self.assertEqual(0, len(errors))
            self.assertEqual(['apples.beancount'],
                             list(map(path.basename, options_map['include'])))

    def test_load_file_nonexist(self):
        entries, errors, options_map = loader.load_file('/bull/bla/root.beancount')
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'does not exist')
        self.assertEqual([], list(map(path.basename, options_map['include'])))

    def test_load_file_with_nonexist_include(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'root.beancount': """
                  include "/some/file/that/does/not/exist.beancount"
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'root.beancount'))
            self.assertEqual(1, len(errors))
            self.assertRegex(errors[0].message, 'does not (match any|exist)')
        self.assertEqual(['root.beancount'],
                         list(map(path.basename, options_map['include'])))

    def test_load_file_with_absolute_include(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "{root}/fruits/oranges.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'fruits/oranges.beancount': """
                  2014-01-02 open Assets:Oranges
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'apples.beancount'))
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))
        self.assertEqual(['apples.beancount', 'oranges.beancount'],
                         list(map(path.basename, options_map['include'])))

    def test_load_file_with_relative_include(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "fruits/oranges.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'fruits/oranges.beancount': """
                  2014-01-02 open Assets:Oranges
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'apples.beancount'))
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))
        self.assertEqual(['apples.beancount', 'oranges.beancount'],
                         list(map(path.basename, options_map['include'])))

    def test_load_file_with_multiple_includes(self):
        # Including recursive includes and mixed and absolute.
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "fruits/oranges.beancount"
                  include "{root}/legumes/patates.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'fruits/oranges.beancount': """
                  include "../legumes/tomates.beancount"
                  2014-01-02 open Assets:Oranges
                """,
                'legumes/tomates.beancount': """
                  2014-01-03 open Assets:Tomates
                """,
                'legumes/patates.beancount': """
                  2014-01-04 open Assets:Patates
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'apples.beancount'))
        self.assertFalse(errors)
        self.assertEqual(4, len(entries))
        self.assertEqual(['apples.beancount', 'oranges.beancount',
                          'patates.beancount', 'tomates.beancount'],
                         list(map(path.basename, options_map['include'])))

    def test_load_file_with_duplicate_includes(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "fruits/oranges.beancount"
                  include "{root}/legumes/tomates.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'fruits/oranges.beancount': """
                  include "../legumes/tomates.beancount"
                  2014-01-02 open Assets:Oranges
                """,
                'legumes/tomates.beancount': """
                  2014-01-03 open Assets:Tomates
                """,
                'legumes/patates.beancount': """
                  2014-01-04 open Assets:Patates
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'apples.beancount'))
        self.assertTrue(errors)
        self.assertEqual(3, len(entries))
        self.assertEqual(['apples.beancount', 'oranges.beancount', 'tomates.beancount'],
                         list(map(path.basename, options_map['include'])))

    def test_load_string_with_relative_include(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "fruits/oranges.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'fruits/oranges.beancount': """
                  2014-01-02 open Assets:Oranges
                """})
            try:
                cwd = os.getcwd()
                os.chdir(tmp)
                entries, errors, options_map = loader.load_file(
                    path.join(tmp, 'apples.beancount'))
            finally:
                os.chdir(cwd)
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))
        self.assertEqual(['apples.beancount', 'oranges.beancount'],
                         list(map(path.basename, options_map['include'])))

    def test_load_file_return_include_filenames(self):
        # Also check that they are normalized paths.
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "oranges.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'oranges.beancount': """
                  include "bananas.beancount"
                  2014-01-02 open Assets:Oranges
                """,
                'bananas.beancount': """
                  2014-01-02 open Assets:Bananas
                """})
            entries, errors, options_map = loader.load_file(
                path.join(tmp, 'apples.beancount'))
        self.assertFalse(errors)
        self.assertEqual(3, len(entries))
        self.assertTrue(all(path.isabs(filename)
                            for filename in options_map['include']))
        self.assertEqual(['apples.beancount', 'bananas.beancount', 'oranges.beancount'],
                         list(map(path.basename, options_map['include'])))


class TestLoadIncludesEncrypted(encryption_test.TestEncryptedBase):

    def test_include_encrypted(self):
        with test_utils.tempdir() as tmpdir:
            test_utils.create_temporary_files(tmpdir, {
                'apples.beancount': """
                  include "oranges.beancount.asc"
                  2014-01-01 open Assets:Apples
                """,
                'oranges.beancount': """
                  2014-01-02 open Assets:Oranges
                """})

            # Encrypt the oranges file and remove the unencrypted file.
            with open(path.join(tmpdir, 'oranges.beancount')) as infile:
                self.encrypt_as_file(infile.read(),
                                     path.join(tmpdir, 'oranges.beancount.asc'))
            os.remove(path.join(tmpdir, 'oranges.beancount'))

            # Load the top-level file which includes the encrypted file.
            with test_utils.environ('GNUPGHOME', self.ringdir):
                entries, errors, options_map = loader.load_file(
                    path.join(tmpdir, 'apples.beancount'))

        self.assertFalse(errors)
        self.assertEqual(2, len(entries))
        self.assertRegex(entries[0].meta['filename'], 'apples.beancount')
        self.assertRegex(entries[1].meta['filename'], 'oranges.+count.asc')


class TestLoadCache(unittest.TestCase):

    def setUp(self):
        self.num_calls = 0
        cache_getter = functools.partial(loader.get_cache_filename,
                                         loader.PICKLE_CACHE_FILENAME)
        mock.patch('beancount.loader._load_file',
                   loader.pickle_cache_function(cache_getter,
                                                0,  # No time threshold.
                                                self._load_file)).start()
    def tearDown(self):
        mock.patch.stopall()

    def _load_file(self, filename, *args, **kw):
        self.num_calls += 1
        return loader._load([(filename, True)], *args, **kw)

    def test_load_cache(self):
        # Create an initial set of files and load file, thus creating a cache.
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "oranges.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'oranges.beancount': """
                  include "bananas.beancount"
                  2014-01-02 open Assets:Oranges
                """,
                'bananas.beancount': """
                  2014-01-02 open Assets:Bananas
                """})
            top_filename = path.join(tmp, 'apples.beancount')
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertFalse(errors)
            self.assertEqual(3, len(entries))
            self.assertEqual(1, self.num_calls)

            # Make sure the cache was created.
            self.assertTrue(path.exists(path.join(tmp, '.apples.beancount.picklecache')))

            # Load the root file again, make sure the cache is being hit.
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertEqual(1, self.num_calls)

            # Touch the top-level file and ensure it's a cache miss.
            with open(top_filename, 'a') as file:
                file.write('\n')
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertEqual(2, self.num_calls)

            # Load the root file again, make sure the cache is being hit.
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertEqual(2, self.num_calls)

            # Touch the top-level file and ensure it's a cache miss.
            with open(top_filename, 'a') as file:
                file.write('\n')
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertEqual(3, self.num_calls)

    def test_load_cache_moved_file(self):
        # Create an initial set of files and load file, thus creating a cache.
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "oranges.beancount"
                  2014-01-01 open Assets:Apples
                """,
                'oranges.beancount': """
                  2014-01-02 open Assets:Oranges
                """})
            top_filename = path.join(tmp, 'apples.beancount')
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertFalse(errors)
            self.assertEqual(2, len(entries))
            self.assertEqual(1, self.num_calls)

            # Make sure the cache was created.
            self.assertTrue(path.exists(path.join(tmp, '.apples.beancount.picklecache')))

            # Check that it doesn't need refresh
            self.assertFalse(loader.needs_refresh(options_map))

            # Move the input file.
            new_top_filename = path.join(tmp, 'bigapples.beancount')
            os.rename(top_filename, new_top_filename)

            # Check that it needs refresh.
            self.assertTrue(loader.needs_refresh(options_map))

            # Load the root file again, make sure the cache is being hit.
            entries, errors, options_map = loader.load_file(top_filename)
            self.assertEqual(2, self.num_calls)

    @mock.patch('os.remove', side_effect=OSError)
    @mock.patch('logging.warning')
    def test_load_cache_read_only_fs(self, remove_mock, warn_mock):
        # Create an initial set of files and load file, thus creating a cache.
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  2014-01-01 open Assets:Apples
                """})
            filename = path.join(tmp, 'apples.beancount')
            entries, errors, options_map = loader.load_file(filename)
            with open(filename, 'w'): pass
            entries, errors, options_map = loader.load_file(filename)
            self.assertEqual(1, len(warn_mock.mock_calls))

    @mock.patch('beancount.loader.PICKLE_CACHE_THRESHOLD', 0.0)
    @mock.patch.object(loader, 'load_file', loader.load_file)
    def test_load_cache_override_filename_pattern_by_env_var(self):
        with test_utils.environ('BEANCOUNT_LOAD_CACHE_FILENAME', '__{filename}__'):
            loader.initialize(use_cache=True)
            with test_utils.tempdir() as tmp:
                test_utils.create_temporary_files(tmp, {
                    'apples.beancount': """
                      2014-01-01 open Assets:Apples
                    """})
                filename = path.join(tmp, 'apples.beancount')
                entries, errors, options_map = loader.load_file(filename)
                self.assertEqual({'__apples.beancount__', 'apples.beancount'},
                                 set(os.listdir(tmp)))

    @mock.patch('beancount.loader.PICKLE_CACHE_THRESHOLD', 0.0)
    @mock.patch.object(loader, 'load_file', loader.load_file)
    def test_load_cache_override_filename_pattern_by_argument(self):
        with test_utils.tempdir() as tmp:
            cache_filename = path.join(tmp, "__{filename}__")
            loader.initialize(use_cache=True, cache_filename=cache_filename)
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  2014-01-01 open Assets:Apples
                """})
            filename = path.join(tmp, 'apples.beancount')
            entries, errors, options_map = loader.load_file(filename)
            self.assertEqual({'__apples.beancount__', 'apples.beancount'},
                             set(os.listdir(tmp)))

    @mock.patch('beancount.loader.PICKLE_CACHE_THRESHOLD', 0.0)
    @mock.patch.object(loader, 'load_file', loader.load_file)
    def test_load_cache_disable(self):
        with test_utils.tempdir() as tmp:
            cache_filename = path.join(tmp, "__{filename}__")
            for kwargs in [dict(use_cache=False),
                           dict(use_cache=False, cache_filename=cache_filename)]:
                loader.initialize(**kwargs)
                test_utils.create_temporary_files(tmp, {
                    'apples.beancount': """
                      2014-01-01 open Assets:Apples
                    """})
                filename = path.join(tmp, 'apples.beancount')
                entries, errors, options_map = loader.load_file(filename)
                self.assertEqual({'apples.beancount'}, set(os.listdir(tmp)))


class TestEncoding(unittest.TestCase):

    def test_string_unicode(self):
        utf8_bytes = textwrap.dedent("""
          2015-01-01 open Assets:Something
          2015-05-23 note Assets:Something "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ "
        """).encode('utf-8')
        entries, errors, options_map = loader.load_string(utf8_bytes, encoding='utf8')
        self.assertFalse(errors)

    def test_string_latin1(self):
        utf8_bytes = textwrap.dedent("""
          2015-01-01 open Assets:Something
          2015-05-23 note Assets:Something "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ "
        """).encode('latin1')
        entries, errors, options_map = loader.load_string(utf8_bytes, encoding='latin1')
        self.assertFalse(errors)


class TestOptionsAggregation(unittest.TestCase):

    def test_aggregate_operating_currencies(self):
        with test_utils.tempdir() as tmp:
            test_utils.create_temporary_files(tmp, {
                'apples.beancount': """
                  include "oranges.beancount"
                  include "bananas.beancount"
                  option "operating_currency" "USD"
                """,
                'oranges.beancount': """
                  option "operating_currency" "CAD"
                """,
                'bananas.beancount': """
                  option "operating_currency" "EUR"
                """})
            top_filename = path.join(tmp, 'apples.beancount')
            entries, errors, options_map = loader.load_file(top_filename)

            self.assertEqual({'USD', 'EUR', 'CAD'}, set(options_map['operating_currency']))


if __name__ == '__main__':
    unittest.main()
