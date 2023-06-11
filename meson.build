project('beancount', 'cpp',
        version : '2.90.0',
        license : 'GPL-v2-only',
        default_options: [
          'cpp_std=c++17',
          'optimization=2',
          'default_library=static',
        ])

absl_container_dep = dependency('absl_container')
absl_hash_dep = dependency('absl_hash')
absl_status_dep = dependency('absl_status')
absl_strings_dep = dependency('absl_strings')
absl_time_dep = dependency('absl_time')

mpdecimal_dep = dependency('mpdecpp')

re2_dep = dependency('re2')

pybind11_dep = dependency('pybind11')

gtest = subproject('gtest')
gtest_main_dep = gtest.get_variable('gtest_main_dep')
gmock_dep = gtest.get_variable('gmock_dep')

# FIXME: Fix protobuf wrap to do not require this. Also update to latest protobuf release.
protobuf_dep = dependency('protobuf', required : false)
if protobuf_dep.found()
  protoc = find_program('protoc')
else
  protobuf = subproject('protobuf')
  protobuf_dep = protobuf.get_variable('protobuf_dep')
  protoc = protobuf.get_variable('protoc')
endif

python = import('python').find_installation()
py3_dep = python.dependency()

bison = find_program('bison', version : '>3.7')

reflex_dep = dependency('reflex')
reflex = find_program('reflex')

cc = meson.get_compiler('cpp')

# Silence warnings from reflex headers
add_project_arguments(cc.get_supported_arguments('-Wno-non-virtual-dtor'), language : 'cpp')

deps = [
  absl_container_dep,
  absl_hash_dep,
  absl_status_dep,
  absl_strings_dep,
  absl_time_dep,
  mpdecimal_dep,
  protobuf_dep,
  re2_dep,
  reflex_dep,
]

includes = include_directories('.')

proto = files(
  'beancount/ccore/data.proto',
  'beancount/ccore/date.proto',
  'beancount/ccore/number.proto',
  'beancount/cparser/ledger.proto',
  'beancount/cparser/options.proto',
  'beancount/cparser/parser.proto',
)

protoc_gen = generator(protoc,
  output : ['@BASENAME@.pb.cc', '@BASENAME@.pb.h'],
  arguments : ['--proto_path=@CURRENT_SOURCE_DIR@', '--cpp_out=@BUILD_DIR@', '@INPUT@'])

proto_cc = protoc_gen.process(proto,
  preserve_path_from : meson.current_source_dir())

subdir('beancount/cparser')

# reflex_gen = generator(reflex,
#   output : ['@BASENAME@.cc', '@BASENAME@.h'],
#   arguments : ['--unicode', '--full', '--header-file=@OUTPUT1@', '-o', '@OUTPUT0@', '@INPUT@'])

# scanner_cc = reflex_gen.process('beancount/cparser/scanner.lxx',
#   preserve_path_from : meson.current_source_dir())

# bison_gen = generator(bison,
#   output : ['@BASENAME@.cc', '@BASENAME@.h'],
#   arguments : ['--report=itemset', '--verbose', '-Wall', '-Werror',
#                '--defines=@OUTPUT1@',
#                '--define=api.location.file="location.h"',
#                '-o', '@OUTPUT0@', '@INPUT@'])

# parser_cc = bison_gen.process('beancount/cparser/parser.yxx',
#   preserve_path_from : meson.current_source_dir())

sources = files(
  'beancount/ccore/account.cc',
  'beancount/ccore/account_types.cc',
  'beancount/ccore/data.cc',
  'beancount/ccore/date.cc',
  'beancount/ccore/inventory.cc',
  'beancount/ccore/number.cc',
  'beancount/ccore/std_utils.cc',
  'beancount/cparser/builder.cc',
  'beancount/cparser/ledger.cc',
  'beancount/cparser/test_utils.cc',
)

cbeancount = static_library('cbeancount',
  sources, proto_cc, scanner_cc, parser_cc,
  include_directories : includes,
  dependencies : deps)

tests = [
  'beancount/ccore/account_test.cc',
  'beancount/ccore/date_test.cc',
  'beancount/ccore/number_test.cc',
  'beancount/ccore/std_utils_test.cc',
  'beancount/cparser/parser_test.cc',
  'beancount/cparser/scanner_test.cc',
  'beancount/cparser/test_utils_test.cc',
]

foreach test : tests
  t = executable(
    test.underscorify(),
    test,
    include_directories : [
      includes,
      cbeancount.private_dir_include()
    ],
    link_with : cbeancount,
    dependencies : [
      deps,
      gmock_dep,
      gtest_main_dep,
    ])

  test(test.split('.')[0], t, protocol : 'gtest')
endforeach

python.extension_module(
  'beancount.ccore._core',
  'beancount/ccore/_core.cc',
  include_directories : [
    includes,
    cbeancount.private_dir_include()
  ],  
  link_with : cbeancount,
  dependencies : [
    deps,
    py3_dep,
    pybind11_dep,
  ],
  override_options : ['b_lto=true'])

python.extension_module(
  'beancount.cparser.extmodule',
  'beancount/cparser/extmodule.cc',
  include_directories : [
    includes,
    cbeancount.private_dir_include()
  ],  
  link_with : cbeancount,
  dependencies : [
    deps,
    py3_dep,
    pybind11_dep,
  ],
  override_options : ['b_lto=true'])
