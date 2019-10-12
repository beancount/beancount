__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.utils import test_utils

from ledgerhub.scripts import remove_crdb


class TestScriptRemoveCRDB(test_utils.TestCase):

    @test_utils.docfile
    def test_success(self, filename):
        """
        Description,Amount
        Bla bla,1029.02 CR
        Bla bla,928.02 DB
        """
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(remove_crdb.main, [filename])

        r = self.assertLines("""
        Description,Amount
        Bla bla,1029.02
        Bla bla,-928.02
        """, stdout.getvalue())


if __name__ == '__main__':
    main()
