STDLIB_DIR := ../stdlib_ml
LIBS := -I lib -I $(STDLIB_DIR)
REPLACE_STDLIB := -nopervasives -nostdlib -open Stdlib
LINK_LIBS := $(STDLIB_DIR)/stdlib.cmo lib/mocha.cmo

TESTS := $(wildcard *.ml)
TESTS_JS := $(patsubst %.ml,%.unlog.as.js,$(TESTS))

test: test_ml test_js
	$(MAKE) -k _run_test

_run_test: run_test_ml run_test_js

run_test_ml:
	@./test_ml

test_ml: $(LINK_LIBS) $(patsubst %.ml,%.cmo,$(TESTS)) | lib/mocha.cmi
	ocamlfind ocamlc -package alcotest -linkpkg -g $(LIBS) $^ -o $@

run_test_js: $(TESTS_JS)
	@echo -e "\n*** Testing JS-generated tests ***\n"
	@../../node_modules/mocha/bin/mocha -R list $^

test_js: $(TESTS_JS)

%.unlog.js: %.ml ../main.byte ../monad_ppx.byte
	../main.byte -mode unlog -ppx ../monad_ppx.byte $(LIBS) $<

%.log.js: %.ml ../main.byte ../monad_ppx.byte
	../main.byte -mode log -ppx ../monad_ppx.byte $(LIBS) $<

%.token.js: %.ml ../main.byte ../monad_ppx.byte
	../main.byte -mode token -ppx ../monad_ppx.byte $(LIBS) $<

%.as.js: $(patsubst %.cmo,%.js,$(LINK_LIBS)) Compare.js %.js
	../assembly.byte -o $@ $^

lib/%.cmi: lib/%.mli
	ocamlfind ocamlc -package alcotest -c $< -o $@

lib/%.cmo: lib/%.ml lib/%.cmi
	ocamlfind ocamlc -package alcotest -c -I lib $< -o $@

%.cmo: %.ml
	ocamlc -c -g $(REPLACE_STDLIB) $(LIBS) $< -o $@

clean:
	rm -rf *.{cmi,cmo} test_ml _build lib/*.{cmi,cmo}

.PRECIOUS: %.js %.unlog.js
.NOTPARALLEL:
