.PHONY: all clean

# requires: opam switch 4.02.1; eval `opam config env`

STD_DIR	    := stdlib_ml
TEST_DIR    := tests
ML_TESTS    := $(wildcard $(TEST_DIR)/*.ml)
ML_LAMBDA   := $(wildcard $(TEST_DIR)/lambda/*.ml)
ML_JSREF    := $(wildcard $(TEST_DIR)/jsref/*.ml)

OCAMLBUILD  := ocamlbuild -j 4 -classic-display -use-ocamlfind

# Used for stdlib and generator dependency generation
CC          := ocamlc -c
OCAMLDEP    := ocamldep -one-line
DEPSED      := sed -e "s/cmo/log.js/; s/cmo/cmi/g; s/cmx/cmi/g"

all: main.byte

debug: main.d.byte

%.byte: *.ml _tags
	$(OCAMLBUILD) $@

native: _tags
	$(OCAMLBUILD) main.native

stdlib:
	$(CC) stdlib_ml/stdlib.mli

%.inferred.mli: _tags
	$(OCAMLBUILD) $@
	cp _build/$@ .

# In case we want to rebuild any .v, but we're likely to be modifying the resulting .mls now anyway...
.PRECIOUS: tests/%.ml
tests/%.ml: tests/%.v
	$(MAKE) -C $(CURDIR)/../../../lib/tlc/src
	cd $(<D) && coqc -I $(CURDIR)/../../../lib/tlc/src $(<F)
	cd $(@D) && rm *.mli
	cd $(@D) && $(CURDIR)/../../ml-add-cstr-annots.pl *.ml

.PRECIOUS: tests/jsref/%.ml
tests/jsref/%.ml:
	$(MAKE) -C $(CURDIR)/../../.. interpreter
	cp ../../../interp/src/extract/*.ml tests/jsref/
	../../convert-ml-strings.pl tests/jsref/*.ml
	cd $(@D) && $(CURDIR)/../../ml-add-cstr-annots.pl *.ml

tests/%.ml.d: tests/%.ml
	$(OCAMLDEP) -I $(<D) $< | $(DEPSED) > $@

tests/%.cmi tests/%.log.js tests/%.unlog.js: tests/%.ml main.byte stdlib
	./main.byte -I $(<D) $<

tests: $(ML_TESTS:.ml=.log.js)

tests/lambda: tests/lambda/Lambda.log.js
tests/jsref: tests/jsref/JsInterpreter.log.js

clean_stdlib:
	rm -f $(STD_DIR)/*.cmi

DIRTY_EXTS := cmi,js.pre,js,d
clean_tests:
	rm -f $(TEST_DIR)/*.{$(DIRTY_EXTS)}
	rm -f $(TEST_DIR)/lambda/*.{$(DIRTY_EXTS)}

clean:
	rm -rf _build
	rm -f *.native *.byte

clean_cmi: clean_tests clean_stdlib
cleanall: clean clean_cmi

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
-include $(ML_TESTS:.ml=.ml.d)
endif

ifeq ($(MAKECMDGOALS),tests/lambda)
-include $(ML_LAMBDA:.ml=.ml.d)
endif

ifeq ($(MAKECMDGOALS),tests/jsref)
-include $(ML_JSREF:.ml=.ml.d)
endif
