export OCAMLMAKEFILE = ../OCamlMakefile

export INCDIRS = ../lib
export LIBDIRS = ../lib
export LIBS = bin_prot
export PACKS = bigarray oUnit

define PROJ_sub1
  SOURCES = bin_prot_test.ml test.ml test_runner.ml
  RESULT = test_runner
endef
export PROJ_sub1

define PROJ_sub2
  SOURCES = mac_test.ml
  RESULT = mac_test
endef
export PROJ_sub2

ifndef SUBPROJS
  export SUBPROJS = sub1 sub2
endif

all:	bc

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

example.pp.ml: example.ml
	camlp4o -o example.pp.ml -I ../../../lib pa_type_conv.cmo pa_bin_prot.cmo example.ml 
