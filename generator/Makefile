	#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
# requires: opam switch 4.02.3; eval `opam config env`

# TODO: test/lambda is not longer supported

###############################################################
# Paths

all: everything
init:
	opam switch 4.02.3
	eval `opam config env`
	opam pin -yn add jsjsref .
	opam install -y jsjsref --deps-only


STDLIB_DIR  := stdlib_ml
TESTS_DIR   := tests
JSREF_DIR   := jsref
JSREF_PATH  := $(TESTS_DIR)/$(JSREF_DIR)
JSREF_ML    := $(filter-out JsOutput.ml,$(wildcard $(JSREF_PATH)/*.ml))
JSREF_MLI   := $(wildcard $(JSREF_PATH)/*.mli)


###############################################################

# ASSEMBLY_JS must respect dependencies order
ASSEMBLY_JS_FILES := \
	BinNums.unlog.js \
	Bool0.unlog.js \
	List0.unlog.js \
	Datatypes.unlog.js \
	Fappli_IEEE_bits.unlog.js \
	Fappli_IEEE.unlog.js \
	LibList.unlog.js \
	LibOption.unlog.js \
	LibProd.unlog.js \
	StdMap.unlog.js \
	Heap.unlog.js \
	HeapInt.unlog.js \
	HeapStr.unlog.js \
	HeapList.unlog.js \
	Shared.unlog.js \
	Compare.js \
	Debug.js \
	JsNumber.js \
	JsSyntax.unlog.js \
	JsSyntaxAux.unlog.js \
	HeapObj.unlog.js \
	Translate_syntax.js \
	JsCommon.unlog.js \
	JsCommonAux.unlog.js \
	JsPreliminary.unlog.js \
	JsInit.unlog.js \
	JsInterpreterMonads.unlog.js \
	JsInterpreter.log.js \
	ModuleExport.js
ASSEMBLY_JS := $(STDLIB_DIR)/stdlib.js $(addprefix tests/jsref/,$(ASSEMBLY_JS_FILES))

#	


###############################################################

DISPLAYED_FILES := \
	JsInterpreter.ml

DISPLAYED := $(addprefix tests/jsref/,$(DISPLAYED_FILES))

ALL_DISPLAYED := $(DISPLAYED:.ml=.unlog.js) $(DISPLAYED:.ml=.pseudo.js) $(DISPLAYED)

ALL_LINEOF := $(DISPLAYED:.ml=.token.js) $(DISPLAYED:.ml=.mlloc.js) $(DISPLAYED:.ml=.ptoken.js)


###############################################################
# Global options

.PHONY: all clean .log.js .unlog.js .token.js .mlloc.js .ptoken.js .pseudo.js
   # all gen log unlog 

# Do not delete intermediate files.
.SECONDARY:


###############################################################
# Tools

CC          := ocamlc -c
OCAMLDEP    := ocamldep -one-line
OCAMLBUILD := ocamlbuild -j 4 -classic-display -use-ocamlfind -X tests -X $(STDLIB_DIR)

OCAMLPAR := OCAMLRUNPARAM="l=200M"

LINEOF := $(OCAMLPAR) ./lineof.byte
MLTOJS := $(OCAMLPAR) ./main.byte -ppx ./monad_ppx.native
# -dsource is automatically considered by main.byte


DISPLAYGEN := $(OCAMLPAR) ./displayed_sources.byte

###############################################################
# Dependencies

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
#-include $(JSREF_ML:.ml=.ml.d)
include $(JSREF_PATH)/.depends
endif


###############################################################
# Rules



##### Compilation of STDLIB

$(STDLIB_DIR)/stdlib.cmi: $(STDLIB_DIR)/stdlib.mli
	$(CC) $<

##### Rule for parser extension

monad_ppx.native: monad_ppx.ml
	$(OCAMLBUILD) $@

##### Rule for binaries

%.byte: *.ml _tags monad_ppx.native
	$(OCAMLBUILD) $@

##### Rule for dependencies

$(JSREF_PATH)/.depends: $(JSREF_ML)
	$(OCAMLDEP) -all -I $(<D) $(<D)/* > $@


##### Rule for cmi
tests/%.cmi: tests/%.ml main.byte stdlib
	$(MLTOJS) -mode cmi -I $(<D) $<

tests/%.cmi: tests/%.mli stdlib
	ocamlc -I $(JSREF_PATH) -I stdlib_ml -open Stdlib $<

##### Rule for log/unlog/token

tests/%.log.js: tests/%.ml main.byte stdlib tests/%.cmi
	$(MLTOJS) -mode log -I $(<D) $<

tests/%.unlog.js: tests/%.ml main.byte stdlib tests/%.cmi
	$(MLTOJS) -mode unlog -I $(<D) $<

tests/%.token.js tests/%.mlloc.js: tests/%.ml main.byte stdlib tests/%.cmi
	$(MLTOJS) -mode token -I $(<D) $<

tests/%.pseudo.js: tests/%.ml main.byte stdlib tests/%.cmi
	$(MLTOJS) -mode pseudo -I $(<D) $<

tests/%.ptoken.js: tests/%.ml main.byte stdlib tests/%.cmi
	$(MLTOJS) -mode ptoken -I $(<D) $<

##### Rule for lineof.js

$(JSREF_PATH)/lineof.js: lineof.byte $(ALL_LINEOF)
	./lineof.byte -o $@ $(ALL_LINEOF)

##### Rule for assembly.js

#--LATER (optional) add as dependencies the unlog files: $(JSREF_ML:.ml=.unlog.js)

$(JSREF_PATH)/assembly.js: assembly.byte $(ASSEMBLY_JS)
	./assembly.byte -o $@ $(ASSEMBLY_JS)
# -stdlib $(STDLIB_DIR)/stdlib.js 

##### Rule for displayed_sources.js

$(JSREF_PATH)/displayed_sources.js: displayed_sources.byte  $(ALL_DISPLAYED)
	$(DISPLAYGEN) -o $@ $(ALL_DISPLAYED)


pseudo: tests/jsref/JsInterpreter.pseudo.js tests/jsref/JsInterpreter.ptoken.js



#### maybe useful ??

tests/jsref/%.log.js: tests/jsref/%.ml 


#####################################################################
# Short targets

everything: assembly lineof display

main: main.byte

cmi: $(JSREF_ML:.ml=.cmi) $(JSREF_MLI:.mli=.cmi) 

gen: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) $(JSREF_ML:.ml=.token.js)

ref: $(JSREF_PATH)/JsInterpreter.log.js $(JSREF_PATH)/JsInterpreter.unlog.js $(JSREF_PATH)/JsInterpreter.token.js

log: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.token.js)

unlog: $(JSREF_ML:.ml=.unlog.js) 

lineof: $(JSREF_PATH)/lineof.js

assembly: $(JSREF_PATH)/assembly.js

display: $(JSREF_PATH)/displayed_sources.js

stdlib: $(STDLIB_DIR)/stdlib.cmi



#####################################################################
# Clean

DIRTY_EXTS := cmi,.mlloc.js,token.js,log.js,unlog.js,d,ml.d,mli.d,js.pre

clean_genjs:
	rm -f $(JSREF_PATH)/lineof.js
	rm -f $(JSREF_PATH)/assembly.js

clean_tests:
	bash -c "rm -f $(JSREF_PATH)/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(JSREF_PATH)/.depends"

#	bash -c "rm -f $(TESTS_DIR)/*.{$(DIRTY_EXTS)}"

clean_stdlib:
	rm -f $(STDLIB_DIR)/*.cmi

clean: clean_genjs clean_tests clean_stdlib
	rm -rf _build
	bash -c "rm -rf .ocamldebug"
	rm -f *.native *.byte



#####################################################################
# Extra

debug: main.d.byte .ocamldebug

.ocamldebug: _tags
	grep -o "package([^)]*)" _tags | sed "s/package(\([^)]*\))/\1/" | xargs ocamlfind query -recursive | sed "s/^/directory /" > .ocamldebug

native: _tags
	$(OCAMLBUILD) main.native

##### Shorthand

tests/%.all: tests/%.log.js tests/%.unlog.js tests/%.token.js
	touch $@

#####################################################################
# Original Build of JSRef Coq to "Humanified" OCaml

#tests/%.ml: tests/%.v
#	$(MAKE) -C $(CURDIR)/../../lib/tlc/src
#	cd $(<D) && coqc -I $(CURDIR)/../../lib/tlc/src $(<F)
#	cd $(@D) && rm *.mli
#	cd $(@D) && $(CURDIR)/../ml-add-cstr-annots.pl *.ml

#$(JSREF_PATH)/%.ml:
#	$(MAKE) -C $(CURDIR)/../.. interpreter
#	cp ../../interp/src/extract/*.ml $(JSREF_PATH)/
#	../convert-ml-strings.pl $(JSREF_PATH)/*.ml
#	cd $(@D) && $(CURDIR)/../ml-add-cstr-annots.pl *.ml
