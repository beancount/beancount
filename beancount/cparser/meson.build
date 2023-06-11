scanner_cc = custom_target('scanner.cc',
  input : 'scanner.lxx',
  output : ['@BASENAME@.cc', '@BASENAME@.h'],
  command : [reflex,
             '--unicode', '--full',
             '--header-file=@OUTPUT1@',
             '-o', '@OUTPUT0@',
             '@INPUT@'])

parser_cc = custom_target('parser.cc',
  input : 'parser.yxx',
  output : ['@BASENAME@.cc', '@BASENAME@.h', 'location.h'],
  command : [bison,
             '-Wall', '-Werror',
             '--defines=@OUTPUT1@',
             '--define=api.location.file="location.h"',
             '-o', '@OUTPUT0@',
             '@INPUT@'])
