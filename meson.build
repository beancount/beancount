project('beancount', 'c', version: files('beancount/VERSION'), meson_version: '>=1.1')

bison = find_program('bison', 'win_bison', version: '>=3.8.0')
flex = find_program('flex', 'win_flex', version: '>=2.6.4')

py = import('python').find_installation(pure: false)

py.install_sources(
    '''
    beancount/__init__.py
    beancount/VERSION
    beancount/py.typed
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
    beancount/utils/defdict.py
    beancount/utils/encryption.py
    beancount/utils/file_utils.py
    beancount/utils/import_utils.py
    beancount/utils/invariants.py
    beancount/utils/memo.py
    beancount/utils/misc_utils.py
    beancount/utils/pager.py
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

# tests
py.install_sources('''
    beancount/core/account_test.py
    beancount/core/account_types_test.py
    beancount/core/amount_test.py
    beancount/core/compare_test.py
    beancount/core/convert_test.py
    beancount/core/data_test.py
    beancount/core/display_context_test.py
    beancount/core/distribution_test.py
    beancount/core/flags_test.py
    beancount/core/getters_test.py
    beancount/core/interpolate_test.py
    beancount/core/inventory_test.py
    beancount/core/number_test.py
    beancount/core/position_test.py
    beancount/core/prices_test.py
    beancount/core/realization_test.py
    beancount/loader_test.py
    beancount/ops/balance_test.py
    beancount/ops/basicops_test.py
    beancount/ops/compress_test.py
    beancount/ops/documents_test.py
    beancount/ops/find_prices_test.py
    beancount/ops/lifetimes_test.py
    beancount/ops/pad_test.py
    beancount/ops/summarize_test.py
    beancount/ops/validation_test.py
    beancount/parser/booking_full_test.py
    beancount/parser/booking_method_test.py
    beancount/parser/booking_test.py
    beancount/parser/cmptest_test.py
    beancount/parser/context_test.py
    beancount/parser/grammar_test.py
    beancount/parser/hashsrc_test.py
    beancount/parser/lexer_test.py
    beancount/parser/options_test.py
    beancount/parser/parser_test.py
    beancount/parser/printer_test.py
    beancount/parser/version_test.py
    beancount/plugins/auto_accounts_test.py
    beancount/plugins/auto_test.py
    beancount/plugins/check_average_cost_test.py
    beancount/plugins/check_closing_test.py
    beancount/plugins/check_commodity_test.py
    beancount/plugins/check_drained_test.py
    beancount/plugins/close_tree_test.py
    beancount/plugins/coherent_cost_test.py
    beancount/plugins/commodity_attr_test.py
    beancount/plugins/currency_accounts_test.py
    beancount/plugins/implicit_prices_test.py
    beancount/plugins/leafonly_test.py
    beancount/plugins/noduplicates_test.py
    beancount/plugins/nounused_test.py
    beancount/plugins/onecommodity_test.py
    beancount/plugins/pedantic_test.py
    beancount/plugins/sellgains_test.py
    beancount/plugins/unique_prices_test.py
    beancount/projects/export_test.py
    beancount/scripts/check_examples_test.py
    beancount/scripts/check_test.py
    beancount/scripts/deps_test.py
    beancount/scripts/directories_test.py
    beancount/scripts/doctor_test.py
    beancount/scripts/example_test.py
    beancount/scripts/format_test.py
    beancount/tools/treeify_test.py
    beancount/utils/bisect_key_test.py
    beancount/utils/defdict_test.py
    beancount/utils/encryption_test.py
    beancount/utils/file_utils_test.py
    beancount/utils/import_utils_test.py
    beancount/utils/invariants_test.py
    beancount/utils/memo_test.py
    beancount/utils/misc_utils_test.py
    beancount/utils/pager_test.py
    beancount/utils/table_test.py
    beancount/utils/test_utils_test.py
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

flex_gen = generator(
    flex,
    output: ['@BASENAME@.c', '@BASENAME@.h'],
    arguments: ['--outfile=@OUTPUT0@', '--header-file=@OUTPUT1@', '@INPUT@']
)

lexer_c = flex_gen.process(
    'beancount/parser/lexer.l',
    preserve_path_from: meson.current_source_dir(),
)

bison_gen = generator(
    bison,
    output: ['@BASENAME@.c', '@BASENAME@.h'],
    arguments: ['--report=itemset', '--verbose', '-Wall', '-Werror', '-o', '@OUTPUT0@', '@INPUT@']
)

grammar_c = bison_gen.process(
    'beancount/parser/grammar.y',
    preserve_path_from: meson.current_source_dir(),
)

parser = py.extension_module(
    '_parser',
    'beancount/parser/decimal.c',
    'beancount/parser/parser.c',
    'beancount/parser/tokens.c',
    lexer_c,
    grammar_c,
    install: true,
    subdir: 'beancount/parser',
)

# Support prehistoric development practices: use ``meson setup build;
# ninja -C build build_ext`` to obtain the equivalent of ``python
# setup.by build_ext --inplace``.
custom_target(
    'extension module in-place',
    input: parser,
    output: 'build_ext',
    command: [
        py,
        '-c',
        'import shutil, sys; shutil.copy(sys.argv[1], sys.argv[2])',
        '@INPUT@',
        meson.project_source_root() / 'beancount/parser/',
    ],
    build_by_default: false,
)

if get_option('tests').enabled()
    cc = meson.get_compiler('c')
    libm = cc.find_library('m', required: false)
    tokens_test = executable(
        'tokens-test',
        'beancount/parser/decimal.c',
        'beancount/parser/tokens.c',
        'beancount/parser/tokens_test.c',
        dependencies: [libm, py.dependency(embed: true)],
        install: false,
    )
    test('tokens', tokens_test)
endif
