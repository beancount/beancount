import tempfile
from os import path

from beancount.utils import test_utils
from beancount.scripts import sql


class TestScriptSQL(test_utils.TestCase):

    def test_example(self):
        # Run the SQL translation on our pretty substantial example file.
        root_dir = test_utils.find_repository_root(__file__)
        tutorial_filename = path.join(root_dir, 'examples/tutorial/example.beancount')

        with tempfile.NamedTemporaryFile('w', suffix='.db', delete=False) as dbfile:
            with test_utils.capture('stdout') as stdout:
                with test_utils.capture('stderr') as stderr:
                    result = test_utils.run_with_args(sql.main,
                                                      [tutorial_filename, dbfile.name])
        self.assertEqual(0, result)
        self.assertNotEqual(0, path.getsize(dbfile.name))


# FIXME: Write a more explicit test with one directive for each and then open
# the DB and make some queries and assert results.
