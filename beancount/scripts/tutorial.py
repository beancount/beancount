"""Write output files for the tutorial commands.
"""
__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import subprocess
import os
from os import path

from beancount.utils import test_utils
from beancount.utils import version


# pylint: disable=bad-whitespace
COMMANDS = [
    ('balances'                 , "bean-report {} balances"),
    ('help-reports'             , "bean-report --help-reports"),
    ('help-subcmd'              , "bean-report {} balances --help"),
    ('help-global'              , "bean-report --help"),
    ('help-formats'             , "bean-report --help-formats"),
    ('balances-restrict'        , "bean-report {} balances -e ETrade"),
    ('balances-restrict-cost'   , "bean-report {} balances -e ETrade -c"),
    ('balances-tree'            , "bean-report {} balances | treeify"),
    ('balsheet'                 , "bean-report {} balsheet"),
    ('journal'                  , ("bean-report {} journal -w 120 "
                                   "-a Assets:US:BofA:Checking")),
    ('journal-with-balance'     , ("bean-report {} journal -w 120 "
                                   "-a Assets:US:BofA:Checking -b")),
    ('invest'                   , ("bean-report {} journal -w 120 "
                                   "-a Assets:US:ETrade:GLD -b")),
    ('invest-with-cost'         , ("bean-report {} journal -w 120 "
                                   "-a Assets:US:ETrade:GLD -b -c")),
    ('journal-unrestricted'     , "bean-report {} journal -w 120 -b"),
    ('holdings'                 , "bean-report {} holdings"),
    ('holdings-by-account'      , "bean-report {} holdings --by account"),
    ('holdings-by-root-account' , "bean-report {} holdings --by root-account"),
    ('holdings-by-commodity'    , "bean-report {} holdings --by commodity"),
    ('holdings-by-currency'     , "bean-report {} holdings --by currency"),
    ('networth'                 , "bean-report {} networth"),
    ('accounts'                 , "bean-report {} accounts"),
    ('events'                   , "bean-report {} events"),
    ('stats-directives'         , "bean-report {} stats-directives"),

    ('stats-postings'           , "bean-report {} stats-postings"),
    ('holdings-csv'             , "bean-report -f csv {} holdings"),
    ]


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = version.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount filename')
    parser.add_argument('output_directory', help='Output directory for the tutorial files')
    args = parser.parse_args()

    for report_name, command_template in COMMANDS:
        logging.info('Generating %s: %s', report_name, command_template)
        output_filename = path.join(args.output_directory, '{}.output'.format(report_name))
        errors_filename = path.join(args.output_directory, '{}.errors'.format(report_name))
        with open(output_filename, 'w') as output_file:
            with open(errors_filename, 'w') as errors_file:
                command = command_template.format(args.filename)
                pipe = subprocess.Popen(command,
                                        env=test_utils.subprocess_env(),
                                        shell=True,
                                        stdout=output_file,
                                        stderr=errors_file)
                pipe.communicate()
                assert pipe.returncode == 0, pipe.returncode

        if path.getsize(errors_filename) == 0:
            os.remove(errors_filename)

    return 0


if __name__ == '__main__':
    main()
