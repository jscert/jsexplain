.PHONY: all clean

# requires: opam switch 4.02.1; eval `opam config env`

STD_DIR     := stdlib_ml
TEST_DIR    := tests
ML_TESTS    := $(wildcard $(TEST_DIR)/*.ml)
ML_LAMBDA   := $(wildcard $(TEST_DIR)/lambda/*.ml)
ML_JSREF    := $(wildcard $(TEST_DIR)/jsref/*.ml) 
MLI_JSREF   := $(wildcard $(TEST_DIR)/jsref/*.mli)

OCAMLBUILD  := ocamlbuild -j 4 -classic-display -use-ocamlfind -X tests -X $(STD_DIR)
# -X $(TEST_DIR)/jsref -X $(TEST_DIR)/lambda 

# Used for stdlib and generator dependency generation
CC          := ocamlc -c
OCAMLDEP    := ocamldep -one-line
# DEPSED      := sed -e "s/cmo/log.js/; s/cmo/cmi/g; s/cmx/cmi/g"
DEPSED      := sed -e "s/cmx/cmi/; s/cmx/cmi/g"

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

.PRECIOUS: tests/jsref/*.ml tests/jsref/*.log.js  tests/jsref/*.unlog.js  tests/jsref/*.token.js
.PHONY: .all .log.js .unlog.js  .token.js

# Do not delete intermediate files.
.SECONDARY:
.PRECIOUS: *.vio



#tests/jsref/%.ml:
#	$(MAKE) -C $(CURDIR)/../../.. interpreter
#	cp ../../../interp/src/extract/*.ml tests/jsref/
#	../../convert-ml-strings.pl tests/jsref/*.ml
#	cd $(@D) && $(CURDIR)/../../ml-add-cstr-annots.pl *.ml

# tests/%.cmi: tests/%.log.js
# tests/jsref/%.cmi: tests/jsref/%.log.js
# tests/jsref/BinNat.cmi : tests/jsref/BinNat.log.js 

tests/%.mli.d: tests/%.mli
	$(OCAMLDEP) -I $(<D) $< | $(DEPSED) > $@

tests/%.ml.d: tests/%.ml
	$(OCAMLDEP) -I $(<D) $< | $(DEPSED) > $@

tests/%.cmi: tests/%.ml main.byte stdlib 
	./main.byte -mode cmi -I $(<D) $<

tests/%.log.js: tests/%.ml main.byte stdlib tests/%.cmi
	./main.byte -mode log -I $(<D) $<

tests/%.unlog.js: tests/%.ml main.byte stdlib tests/%.cmi 
	./main.byte -mode unlog -I $(<D) $<

tests/%.token.js: tests/%.ml main.byte stdlib tests/%.cmi  
	./main.byte -mode token -I $(<D) $<

tests/%.all: tests/%.log.js tests/%.unlog.js tests/%.token.js
	touch $@

#tests/%.cmi: tests/%.ml main.byte stdlib
#	./main.byte -mode unlog -I $(<D) $<


# ad hoc rules

tests/jsref/Translate_syntax.cmi: tests/jsref/Translate_syntax.mli tests/jsref/JsSyntax.cmi stdlib
	ocamlc -I tests/jsref -I stdlib_ml -open Stdlib $<

tests/jsref/Prheap.cmi: tests/jsref/Prheap.mli stdlib tests/jsref/JsSyntax.cmi
	ocamlc -I tests/jsref -I stdlib_ml -open Stdlib $<




# tests/%.cmi: tests/%.mli stdlib
#	ocamlc -I stdlib_ml -open Stdlib -I $(<D) $<


tests: $(ML_TESTS:.ml=.log.js) $(ML_TESTS:.ml=.token.js)

tests/lambda: tests/lambda/Lambda.log.js

tests/jsref: tests/jsref/JsInterpreter.log.js
tests/jsrefunlog: tests/jsref/JsInterpreter.unlog.js


arthur: $(ML_JSREF:.ml=.log.js) $(ML_JSREF:.ml=.unlog.js) $(ML_JSREF:.ml=.token.js)

# foo:
# 	make $(ML_JSREF:.ml=.log.js)
# 
# tests/jsref/JsInterpreter.log.js


######### lineof target #########

tests/jsref/lineof.js: lineof.byte $(ML_JSREF:.ml=.token.js)
	lineof.byte -o $@ $(ML_JSREF:.ml=.token.js)

lineof: tests/jsref/lineof.js

##################


clean_stdlib:
	rm -f $(STD_DIR)/*.cmi

DIRTY_EXTS := cmi,token.js,js,d
clean_tests:
	bash -c "rm -f $(TEST_DIR)/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(TEST_DIR)/lambda/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(TEST_DIR)/jsref/*.{$(DIRTY_EXTS)}"

clean_jsref:
	rm -f tests/jsref/*.ml.d tests/jsref/*.mli.d tests/jsref/*.log.js tests/jsref/*.unlog.js tests/jsref/*.token.js  tests/jsref/*.cmi

clean: clean_jsref
	rm -rf _build
	rm -f *.native *.byte
	rm -f stdlib_ml/*.cmi


clean_cmi: clean_tests clean_stdlib
cleanall: clean clean_cmi

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
-include $(ML_TESTS:.ml=.ml.d)
-include $(ML_JSREF:.ml=.ml.d)
-include $(MLI_JSREF:.mli=.mli.d)
endif


#ifeq ($(findstring clean,$(MAKECMDGOALS)),)
#-include $(ML_JSREF:.ml=.ml.d)
#-include $(MLI_JSREF:.mli=.mli.d)
#endif
