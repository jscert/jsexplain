#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
# requires: opam switch 4.02.1; eval `opam config env`

# TODO: test/lambda is not longer supported

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
# Global options

.PHONY: all clean .log.js .unlog.js .token.js
   # all gen log unlog 

all: everything

# Do not delete intermediate files.
.SECONDARY:


###############################################################
# Tools

CC          := ocamlc -c
OCAMLDEP    := ocamldep -one-line
DEPSED      := sed -e "s/cmx/cmi/; s/cmx/cmi/g"
OCAMLBUILD := ocamlbuild -j 4 -classic-display -use-ocamlfind -X tests -X $(STDLIB_DIR)

GENERATOR := ./main.byte

LINEOF := ./lineof.byte


###############################################################
# Dependencies

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
-include $(TESTS_ML:.ml=.ml.d)
-include $(JSREF_ML:.ml=.ml.d)
-include $(JSREF_MLI:.mli=.mli.d)
endif


###############################################################
# Rules

##### Compilation of STDLIB

$(STDLIB_DIR)/stdlib.cmi: $(STDLIB_DIR)/stdlib.mli
	$(CC) $<

##### Rule for binaries

%.byte: *.ml _tags
	$(OCAMLBUILD) $@

##### Rule for dependencies

tests/%.mli.d: tests/%.mli
	$(OCAMLDEP) -I $(<D) $< | $(DEPSED) > $@

tests/%.ml.d: tests/%.ml
	$(OCAMLDEP) -I $(<D) $< | $(DEPSED) > $@

##### Rule for cmi

tests/%.cmi: tests/%.ml main.byte stdlib 
	./main.byte -mode cmi -I $(<D) $<

##### Custome cmi rules for compilation of mli files without ml source

$(JSREF_PATH)/Translate_syntax.cmi: $(JSREF_PATH)/Translate_syntax.mli $(JSREF_PATH)/JsSyntax.cmi stdlib
	ocamlc -I $(JSREF_PATH) -I stdlib_ml -open Stdlib $<

$(JSREF_PATH)/Prheap.cmi: $(JSREF_PATH)/Prheap.mli stdlib $(JSREF_PATH)/JsSyntax.cmi
	ocamlc -I $(JSREF_PATH) -I stdlib_ml -open Stdlib $<

##### Rule for log/unlog/token

tests/%.log.js: tests/%.ml main.byte stdlib tests/%.cmi
	./main.byte -mode log -I $(<D) $<

tests/%.unlog.js: tests/%.ml main.byte stdlib tests/%.cmi 
	./main.byte -mode unlog -I $(<D) $<

tests/%.token.js: tests/%.ml main.byte stdlib tests/%.cmi  
	./main.byte -mode token -I $(<D) $<

##### Rule for lineof.js

$(JSREF_PATH)/lineof.js: lineof.byte $(JSREF_ML:.ml=.token.js)
	./lineof.byte -o $@ $(JSREF_ML:.ml=.token.js)

##### Rule for assembly.js

$(JSREF_PATH)/assembly.js: assembly.byte $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) 
	./assembly.byte -o $@ -stdlib $(STDLIB_DIR)/stdlib.js $(JSREF_ML:.ml=)


# maybe useful

tests/jsref/%.log.js: tests/jsref/%.ml 


#####################################################################
# Short targets

everything: gen assembly lineof

main: main.byte

cmi: $(JSREF_ML:.ml=.cmi) $(JSREF_MLI:.mli=.cmi) 

gen: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) $(JSREF_ML:.ml=.token.js)

log: $(TESTS_ML:.ml=.log.js) $(TESTS_ML:.ml=.token.js)

unlog: $(JSREF_ML:.ml=.unlog.js) 

lineof: $(JSREF_PATH)/lineof.js

assembly: $(JSREF_PATH)/assembly.js

stdlib: $(STDLIB_DIR)/stdlib.cmi



#####################################################################
# Clean

DIRTY_EXTS := cmi,token.js,log.js,unlog.js,d,ml.d,mli.d

clean_genjs:
	rm -f $(JSREF_PATH)/lineof.js
	rm -f $(JSREF_PATH)/assembly.js

clean_tests:
	bash -c "rm -f $(TESTS_DIR)/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(TESTS_DIR)/$(JSREF_DIR)/*.{$(DIRTY_EXTS)}"

clean_stdlib:
	rm -f $(STDLIB_DIR)/*.cmi

clean: clean_genjs clean_tests clean_stdlib
	rm -rf _build
	rm -f *.native *.byte



#####################################################################
# Extra

debug: main.d.byte

native: _tags
	$(OCAMLBUILD) main.native

##### Shorthand

tests/%.all: tests/%.log.js tests/%.unlog.js tests/%.token.js
	touch $@

#####################################################################
# Deprecated

####
#
#%.inferred.mli: _tags
#	$(OCAMLBUILD) $@
#	cp _build/$@ .

# tests/lambda: tests/lambda/Lambda.log.js


#ifeq ($(findstring clean,$(MAKECMDGOALS)),)
#-include $(JSREF_ML:.ml=.ml.d)
#-include $(JSREF_MLI:.mli=.mli.d)
#endif


# ML_LAMBDA   := $(wildcard $(TEST_DIR)/lambda/*.ml)
# DEPSED      := sed -e "s/cmo/log.js/; s/cmo/cmi/g; s/cmx/cmi/g"
# -X $(TESTS_DIR)/jsref -X $(TESTS_DIR)/lambda 


# 	bash -c "rm -f $(TESTS_DIR)/lambda/*.{$(DIRTY_EXTS)}"

# clean_jsref:
#	rm -f $(JSREF_PATH)/*.ml.d $(JSREF_PATH)/*.mli.d $(JSREF_PATH)/*.log.js $(JSREF_PATH)/*.unlog.js $(JSREF_PATH)/*.token.js  $(JSREF_PATH)/*.cmi

# PRECIOUS probably not useful:
# .PRECIOUS: $(JSREF_PATH)/*.ml $(JSREF_PATH)/*.log.js $(JSREF_PATH)/*.unlog.js  $(JSREF_PATH)/*.token.js
# .PRECIOUS: *.vio
#.PRECIOUS: tests/%.ml



#tests/%.ml: tests/%.v
#	$(MAKE) -C $(CURDIR)/../../../lib/tlc/src
#	cd $(<D) && coqc -I $(CURDIR)/../../../lib/tlc/src $(<F)
#	cd $(@D) && rm *.mli
#	cd $(@D) && $(CURDIR)/../../ml-add-cstr-annots.pl *.ml




#$(JSREF_PATH)/%.ml:
#	$(MAKE) -C $(CURDIR)/../../.. interpreter
#	cp ../../../interp/src/extract/*.ml $(JSREF_PATH)/
#	../../convert-ml-strings.pl $(JSREF_PATH)/*.ml
#	cd $(@D) && $(CURDIR)/../../ml-add-cstr-annots.pl *.ml

# tests/%.cmi: tests/%.log.js
# $(JSREF_PATH)/%.cmi: $(JSREF_PATH)/%.log.js
# $(JSREF_PATH)/BinNat.cmi : $(JSREF_PATH)/BinNat.log.js 
