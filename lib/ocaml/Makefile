.PHONY: all
all:
	@cd lib && $(MAKE)
	@cd lib_test && make

.PHONY:	install
install:
	@cd lib && $(MAKE) $@

.PHONY:	uninstall
uninstall:
	@cd lib && $(MAKE) $@

.PHONY:	clean
clean:
	@cd lib && $(MAKE) clean
	@cd lib_test && $(MAKE) clean
