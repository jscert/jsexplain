#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
# requires: opam switch 4.02.1; eval `opam config env`


###############################################################
# Global options

.PHONY: all clean

all:

###############################################################
# Paths

STDLIB_DIR  := stdlib_ml
TESTS_DIR   := tests
JSREF_DIR   := jsref
JSREF_PATH  := $(TESTS_DIR)/$(JSREF_DIR)
TESTS_ML    := $(wildcard $(TESTS_DIR)/*.ml)
JSREF_ML    := $(wildcard $(TESTS_DIR)/$(JSREF_DIR)/*.ml) 
JSREF_MLI   := $(wildcard $(TESTS_DIR)/$(JSREF_DIR)/*.mli)


###############################################################
# Tools

CC          := ocamlc -c
OCAMLDEP    := ocamldep -one-line
DEPSED      := sed -e "s/cmx/cmi/; s/cmx/cmi/g"
OCAMLBUILD := ocamlbuild -j 4 -classic-display -use-ocamlfind -X tests -X $(STDLIB_DIR)

GENERATOR := ./main.byte

LINEOF := ./lineof.byte

# Do not delete intermediate files.
.SECONDARY:
.PRECIOUS: *.vio


###############################################################
# Dependencies

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
-include $(TESTS_ML:.ml=.ml.d)
-include $(JSREF_ML:.ml=.ml.d)
-include $(JSREF_MLI:.mli=.mli.d)
endif


###############################################################
# Short targets

all: main.byte

%.byte: *.ml _tags
	$(OCAMLBUILD) $@

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


tests: $(TESTS_ML:.ml=.log.js) $(TESTS_ML:.ml=.token.js)

tests/lambda: tests/lambda/Lambda.log.js

tests/jsref: tests/jsref/JsInterpreter.log.js
tests/jsrefunlog: tests/jsref/JsInterpreter.unlog.js





######### lineof target #########

tests/jsref/lineof.js: lineof.byte $(ML_JSREF:.ml=.token.js)
	./lineof.byte -o $@ $(ML_JSREF:.ml=.token.js)


#####################################################################
# Short targets

unlog: $(JSREF_ML:.ml=.unlog.js) 
full: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) $(JSREF_ML:.ml=.token.js)
lineof: tests/jsref/lineof.js





#####################################################################
# Clean

DIRTY_EXTS := cmi,token.js,log.js,unlog.js,d,ml.d,mli.d

clean_tests:
	bash -c "rm -f $(TESTS_DIR)/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(TESTS_DIR)/$(JSREF_DIR)/*.{$(DIRTY_EXTS)}"

clean_stdlib:
	rm -f $(STDLIB_DIR)/*.cmi

clean: clean_tests clean_stdlib
	rm -rf _build
	rm -f *.native *.byte

clean_cmi: clean_tests clean_stdlib
cleanall: clean clean_cmi




#####################################################################
# Extra

debug: main.d.byte

native: _tags
	$(OCAMLBUILD) main.native


#####################################################################
# Deprecated

#ifeq ($(findstring clean,$(MAKECMDGOALS)),)
#-include $(JSREF_ML:.ml=.ml.d)
#-include $(JSREF_MLI:.mli=.mli.d)
#endif


# ML_LAMBDA   := $(wildcard $(TEST_DIR)/lambda/*.ml)
# DEPSED      := sed -e "s/cmo/log.js/; s/cmo/cmi/g; s/cmx/cmi/g"
# -X $(TESTS_DIR)/jsref -X $(TESTS_DIR)/lambda 


# 	bash -c "rm -f $(TESTS_DIR)/lambda/*.{$(DIRTY_EXTS)}"

# clean_jsref:
#	rm -f tests/jsref/*.ml.d tests/jsref/*.mli.d tests/jsref/*.log.js tests/jsref/*.unlog.js tests/jsref/*.token.js  tests/jsref/*.cmi
