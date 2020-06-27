"""Write output files for the tutorial commands.
"""
__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import subprocess
import sys
import os
from os import path

from beancount.utils import test_utils
from beancount.utils import version


# pylint: disable=bad-whitespace,line-too-long
COMMANDS = [
    ('balances'                 , "{bindir:}/bean-report {filename:} balances"),
    ('help-reports'             , "{bindir:}/bean-report --help-reports"),
    ('help-subcmd'              , "{bindir:}/bean-report {filename:} balances --help"),
    ('help-global'              , "{bindir:}/bean-report --help"),
    ('help-formats'             , "{bindir:}/bean-report --help-formats"),
    ('balances-restrict'        , "{bindir:}/bean-report {filename:} balances -e ETrade"),
    ('balances-restrict-cost'   , "{bindir:}/bean-report {filename:} balances -e ETrade -c"),
    ('balances-tree'            , "{bindir:}/bean-report {filename:} balances | {bindir:}/treeify"),
    ('balsheet'                 , "{bindir:}/bean-report {filename:} balsheet"),
    ('journal'                  , "{bindir:}/bean-report {filename:} journal -w 120 -a Assets:US:BofA:Checking"),
    ('journal-with-balance'     , "{bindir:}/bean-report {filename:} journal -w 120 -a Assets:US:BofA:Checking -b"),
    ('invest'                   , "{bindir:}/bean-report {filename:} journal -w 120 -a Assets:US:ETrade:GLD -b"),
    ('invest-with-cost'         , "{bindir:}/bean-report {filename:} journal -w 120 -a Assets:US:ETrade:GLD -b -c"),
    ('journal-unrestricted'     , "{bindir:}/bean-report {filename:} journal -w 120 -b"),
    ('holdings'                 , "{bindir:}/bean-report {filename:} holdings"),
    ('holdings-by-account'      , "{bindir:}/bean-report {filename:} holdings --by account"),
    ('holdings-by-root-account' , "{bindir:}/bean-report {filename:} holdings --by root-account"),
    ('holdings-by-commodity'    , "{bindir:}/bean-report {filename:} holdings --by commodity"),
    ('holdings-by-currency'     , "{bindir:}/bean-report {filename:} holdings --by currency"),
    ('networth'                 , "{bindir:}/bean-report {filename:} networth"),
    ('accounts'                 , "{bindir:}/bean-report {filename:} accounts"),
    ('events'                   , "{bindir:}/bean-report {filename:} events"),
    ('stats-directives'         , "{bindir:}/bean-report {filename:} stats-directives"),
    ('stats-postings'           , "{bindir:}/bean-report {filename:} stats-postings"),
    ('holdings-csv'             , "{bindir:}/bean-report -f csv {filename:} holdings"),
]


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = version.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount filename')
    parser.add_argument('output_directory', help='Output directory for the tutorial files')
    args = parser.parse_args()
    rootdir = test_utils.find_repository_root(__file__)
    bindir = "{} {}/bin/".format(sys.executable, rootdir)

    for report_name, command_template in COMMANDS:
        logging.info('Generating %s: %s', report_name, command_template)
        output_filename = path.join(args.output_directory, '{}.output'.format(report_name))
        errors_filename = path.join(args.output_directory, '{}.errors'.format(report_name))
        with open(output_filename, 'w') as output_file:
            with open(errors_filename, 'w') as errors_file:
                command = command_template.format(bindir=bindir, filename=args.filename)
                pipe = subprocess.Popen(command,
                                        env=test_utils.subprocess_env(),
                                        shell=True,
                                        stdout=output_file,
                                        stderr=errors_file)
                pipe.communicate()
                if pipe.returncode != 0:
                    with open(errors_filename) as efile:
                        errors = efile.read()
                    raise RuntimeError(
                        "Error running '{}': exit with {}; errors: {}".format(
                            command, pipe.returncode, errors))

        if path.getsize(errors_filename) == 0:
            os.remove(errors_filename)

    return 0


if __name__ == '__main__':
    main()
