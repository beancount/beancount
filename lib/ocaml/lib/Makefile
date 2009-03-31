OCAMLMAKEFILE = ../OCamlMakefile

SOURCES =					\
  decimal.mli decimal.ml			\
  wallet.mli wallet.ml

LIB_PACK_NAME = beancount
# INCDIRS = +camlp4
# OCAMLFLAGS = -for-pack Bin_prot
CFLAGS = \
  -pipe -g -fPIC -O2 -fomit-frame-pointer \
  -Wall -pedantic -Wextra -Wunused -Werror -Wno-long-long
# PACKS = type-conv
RESULT = beancount

# TRASH = pa_bin_prot.cmi pa_bin_prot.cmo pa_bin_prot.annot

all:						\
	byte-code-library

# 	pack-byte-code pack-native-code		\
# 	byte-code-library native-code-library	\
# 	beancount.cmi beancount.cmo

LIBINSTALL_FILES =				\
  $(SOURCES)					\
  beancount.cmi beancount.cmo

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
