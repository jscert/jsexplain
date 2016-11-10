#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
###############################################################

all: jsjsref mljsref

ML_FRONTEND_SRC := ml_frontend

###############################################################
##### Dependencies

# Avoid dependency generation for non-jsjsref build targets
ifeq ($(filter clean%,$(MAKECMDGOALS)),)
include .depends
endif

.depends: $(JSREF_ML) $(PPX_BIN)
	$(OCAMLDEP) -ppx $(PPX_BIN) -I . -all * $(ML_FRONTEND_SRC)/* > $@

###############################################################
###############################################################
# JS-JSRef
JSREF_ML  := $(wildcard *.ml)
JSREF_MLI := $(wildcard *.mli)

GENERATOR_DIR := ../generator

STDLIB_DIR := $(GENERATOR_DIR)/stdlib_ml
STDLIB_FLAG := -I $(STDLIB_DIR) -open Stdlib
REPLACE_STDLIB := -nopervasives -nostdlib $(STDLIB_FLAG)

# ASSEMBLY_JS_FILES must respect dependencies order
ASSEMBLY_JS_FILES := \
	$(STDLIB_DIR)/stdlib.js \
	BinNums.unlog.js \
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
	JsNumber.js \
	JsSyntax.unlog.js \
	JsSyntaxAux.unlog.js \
	HeapObj.unlog.js \
	Translate_syntax.js \
	JsCommon.unlog.js \
	JsCommonAux.unlog.js \
	JsPreliminary.unlog.js \
	JsInit.unlog.js \
	Prheap.js \
	Debug.js \
	JsInterpreterMonads.unlog.js \
	JsInterpreter.log.js \
	JsInterpreterUtils.unlog.js

ASSEMBLY_JS := \
	$(ASSEMBLY_JS_FILES) \
	ModuleExport.js

###############################################################

DISPLAYED := \
	JsInterpreter.ml

ALL_DISPLAYED := $(DISPLAYED:.ml=.unlog.js) $(DISPLAYED:.ml=.pseudo.js) $(DISPLAYED)

ALL_LINEOF := $(DISPLAYED:.ml=.token.js) $(DISPLAYED:.ml=.mlloc.js) $(DISPLAYED:.ml=.ptoken.js)

###############################################################
##### Tools

OCAMLDEP := ocamldep -one-line
OCAMLPAR := OCAMLRUNPARAM="l=200M"

LINEOF_BIN := $(GENERATOR_DIR)/lineof.byte
LINEOF     := $(OCAMLPAR) $(LINEOF_BIN)

PPX_BIN    := $(GENERATOR_DIR)/monad_ppx.byte
PPX_FLAG   := -ppx $(PPX_BIN)

MLTOJS_BIN := $(GENERATOR_DIR)/main.byte
MLTOJS 	   := $(OCAMLPAR) $(MLTOJS_BIN) $(PPX_FLAG)
# -dsource is automatically considered by main.byte

ASSEMBLY_BIN := $(GENERATOR_DIR)/assembly.byte
ASSEMBLY     := $(ASSEMBLY_BIN)

DISPLAYGEN_BIN := $(GENERATOR_DIR)/displayed_sources.byte
DISPLAYGEN     := $(OCAMLPAR) $(DISPLAYGEN_BIN)

# Warn if buildtools are missing
$(MLTOJS_BIN) $(LINEOF_BIN) $(PPX_BIN) $(ASSEMBLY_BIN) $(DISPLAYGEN_BIN):
	$(error Missing generator tools, build from project root, or set paths.)


###############################################################
# Rules

##### Rule for cmi
# Ordering of these rules is important, mli rule must be first.
%.cmi: %.mli
	ocamlc $(REPLACE_STDLIB) $<

%.cmi: %.ml $(MLTOJS_BIN)
	$(MLTOJS) -mode cmi -I $(STDLIB_DIR) $<

##### Rule for log/unlog/token

%.log.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode log -I $(STDLIB_DIR) $<

%.unlog.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode unlog -I $(STDLIB_DIR) $<

%.token.js %.mlloc.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode token -I $(STDLIB_DIR) $<

%.pseudo.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode pseudo -I $(STDLIB_DIR) $<

%.ptoken.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode ptoken -I $(STDLIB_DIR) $<

##### Rule for lineof.js

lineof.js: $(ALL_LINEOF) $(LINEOF_BIN)
	$(LINEOF) -o $@ $(ALL_LINEOF)

##### Rule for assembly.js

#--LATER (optional) add as dependencies the unlog files: $(JSREF_ML:.ml=.unlog.js)

assembly.js: $(ASSEMBLY_JS) $(ASEMBLY_BIN)
	$(ASSEMBLY) -o $@ $(ASSEMBLY_JS)

##### Rule for displayed_sources.js

displayed_sources.js: $(ALL_DISPLAYED) $(DISPLAYGEN_BIN)
	$(DISPLAYGEN) -o $@ $(ALL_DISPLAYED)

###############################################################
# Short targets

jsjsref: assembly lineof display

cmi: $(JSREF_ML:.ml=.cmi) $(JSREF_MLI:.mli=.cmi) 

gen: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) $(JSREF_ML:.ml=.token.js)

ref: JsInterpreter.log.js JsInterpreter.unlog.js JsInterpreter.token.js

pseudo: JsInterpreter.pseudo.js JsInterpreter.ptoken.js

log: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.token.js)

unlog: $(JSREF_ML:.ml=.unlog.js) 

lineof: lineof.js

assembly: assembly.js

display: displayed_sources.js

DIRTY_EXTS := {log,unlog,token,pseudo,ptoken,mlloc}.js
clean_jsjsref:
	sh -c "rm -f {lineof,assembly,displayed_sources}.js"
	sh -c "rm -f *.$(DIRTY_EXTS)"

.PHONY: jsjsref cmi gen ref pseudo log unlog lineof assembly display clean_jsjsref

###############################################################
###############################################################
# ML-JSRef

# Strip all extensions of $(ASSEMBLY_JS) and replace with .cmo
ASSEMBLY_ML_FILES := $(addsuffix .cmo,$(basename $(basename $(ASSEMBLY_JS_FILES))))
ASSEMBLY_ML := $(ASSEMBLY_ML_FILES) \
	run_js.cmo

mljsref: main.byte

main.byte: $(ASSEMBLY_ML)
	ocamlfind ocamlc -linkpkg -package JS_Parser -o $@ $^

run_js.cmo: run_js.ml run_js.mli run_js.cmi
	ocamlfind ocamlc -c -package JS_Parser $<

Translate_syntax.cmo: Translate_syntax.ml Translate_syntax.mli Translate_syntax.cmi
	ocamlfind ocamlc -c -package JS_Parser $<

# Ordering of the .cmo rules is important. mli dependency forces "standard" compilation
%.cmo: %.ml %.mli %.cmi $(PPX_BIN)
	ocamlc -c $(PPX_FLAG) $<

# Standalone .ml files
%.cmo: %.ml %.cmi $(PPX_BIN)
	ocamlc -c $(REPLACE_STDLIB) $(PPX_FLAG) $<

clean_mljsref:
	sh -c "rm -f $(ML_FRONTEND_SRC)/*.mli"
	sh -c "rm -f {,$(ML_FRONTEND_SRC)/}*.{cmo,byte}"

.PHONY: mljsref clean_mljsref

##############################################################
##############################################################
# Global options

clean: clean_jsjsref clean_mljsref
	sh -c "rm -f *.cmi"
	sh -c "rm -f .depends"

# Phony global targets
.PHONY: all clean

# Do not delete intermediate files.
.SECONDARY:

