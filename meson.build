project('beancount', 'c', version: files('beancount/VERSION'))

py = import('python').find_installation(pure: false)

py.install_sources(
    '''
    beancount/__init__.py
    beancount/VERSION
    beancount/api.py
    beancount/core/__init__.py
    beancount/core/account.py
    beancount/core/account_types.py
    beancount/core/amount.py
    beancount/core/compare.py
    beancount/core/convert.py
    beancount/core/data.py
    beancount/core/display_context.py
    beancount/core/distribution.py
    beancount/core/flags.py
    beancount/core/getters.py
    beancount/core/interpolate.py
    beancount/core/inventory.py
    beancount/core/number.py
    beancount/core/position.py
    beancount/core/prices.py
    beancount/core/realization.py
    beancount/loader.py
    beancount/ops/__init__.py
    beancount/ops/balance.py
    beancount/ops/basicops.py
    beancount/ops/compress.py
    beancount/ops/documents.py
    beancount/ops/find_prices.py
    beancount/ops/lifetimes.py
    beancount/ops/pad.py
    beancount/ops/summarize.py
    beancount/ops/validation.py
    beancount/parser/__init__.py
    beancount/parser/booking.py
    beancount/parser/booking_full.py
    beancount/parser/booking_method.py
    beancount/parser/cmptest.py
    beancount/parser/context.py
    beancount/parser/grammar.py
    beancount/parser/hashsrc.py
    beancount/parser/lexer.py
    beancount/parser/options.py
    beancount/parser/parser.py
    beancount/parser/printer.py
    beancount/parser/version.py
    beancount/plugins/__init__.py
    beancount/plugins/auto.py
    beancount/plugins/auto_accounts.py
    beancount/plugins/check_average_cost.py
    beancount/plugins/check_closing.py
    beancount/plugins/check_commodity.py
    beancount/plugins/check_drained.py
    beancount/plugins/close_tree.py
    beancount/plugins/coherent_cost.py
    beancount/plugins/commodity_attr.py
    beancount/plugins/currency_accounts.py
    beancount/plugins/implicit_prices.py
    beancount/plugins/leafonly.py
    beancount/plugins/noduplicates.py
    beancount/plugins/nounused.py
    beancount/plugins/onecommodity.py
    beancount/plugins/pedantic.py
    beancount/plugins/sellgains.py
    beancount/plugins/unique_prices.py
    beancount/projects/__init__.py
    beancount/projects/export.py
    beancount/scripts/__init__.py
    beancount/scripts/check.py
    beancount/scripts/deps.py
    beancount/scripts/directories.py
    beancount/scripts/doctor.py
    beancount/scripts/example.py
    beancount/scripts/format.py
    beancount/tools/__init__.py
    beancount/tools/treeify.py
    beancount/utils/__init__.py
    beancount/utils/bisect_key.py
    beancount/utils/date_utils.py
    beancount/utils/defdict.py
    beancount/utils/encryption.py
    beancount/utils/file_utils.py
    beancount/utils/import_utils.py
    beancount/utils/invariants.py
    beancount/utils/memo.py
    beancount/utils/misc_utils.py
    beancount/utils/pager.py
    beancount/utils/snoop.py
    beancount/utils/table.py
    beancount/utils/test_utils.py
    '''.split(),
    preserve_path: true,
)

# these are needed only to be able to run hashsrc.py at run time
py.install_sources(
    '''
    beancount/parser/lexer.l
    beancount/parser/grammar.y
    beancount/parser/decimal.h
    beancount/parser/decimal.c
    beancount/parser/macros.h
    beancount/parser/parser.h
    beancount/parser/parser.c
    beancount/parser/tokens.h
    '''.split(),
    preserve_path: true,
)

parser_source_hash = run_command(
    [py, '-c', 'from hashsrc import hash_parser_source_files; print(hash_parser_source_files())'],
    env: {'PYTHONPATH': '@0@/beancount/parser/'.format(meson.current_source_dir())},
    capture: true,
    check: true,
).stdout().strip()

add_project_arguments(
    '-DPARSER_SOURCE_HASH=@0@'.format(parser_source_hash),
    '-DBEANCOUNT_VERSION=@0@'.format(meson.project_version()),
    language: 'c',
)

if host_machine.system() == 'windows'
    add_project_arguments('-DYY_NO_UNISTD_H', language: 'c')
endif

py.extension_module(
    '_parser',
    'beancount/parser/decimal.c',
    'beancount/parser/lexer.c',
    'beancount/parser/grammar.c',
    'beancount/parser/parser.c',
    'beancount/parser/tokens.c',
    install: true,
    subdir: 'beancount/parser',
)
