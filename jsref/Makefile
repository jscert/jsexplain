#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
###############################################################
# Paths

all: # Defined below

JSREF_ML  := $(filter-out JsOutput.ml,$(wildcard *.ml))
JSREF_MLI := $(wildcard *.mli)

GENERATOR_DIR := ../generator
STDLIB_DIR := $(GENERATOR_DIR)/stdlib_ml

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
ASSEMBLY_JS := $(STDLIB_DIR)/stdlib.js $(ASSEMBLY_JS_FILES)


###############################################################

DISPLAYED := \
	JsInterpreter.ml

ALL_DISPLAYED := $(DISPLAYED:.ml=.unlog.js) $(DISPLAYED:.ml=.pseudo.js) $(DISPLAYED)

ALL_LINEOF := $(DISPLAYED:.ml=.token.js) $(DISPLAYED:.ml=.mlloc.js) $(DISPLAYED:.ml=.ptoken.js)

###############################################################
# Tools

OCAMLDEP := ocamldep -one-line
OCAMLPAR := OCAMLRUNPARAM="l=200M"

LINEOF_BIN := $(GENERATOR_DIR)/lineof.byte
LINEOF     := $(OCAMLPAR) $(LINEOF_BIN)

MLTOJS_BIN := $(GENERATOR_DIR)/main.byte
PPX_BIN    := $(GENERATOR_DIR)/monad_ppx.native
MLTOJS 	   := $(OCAMLPAR) $(MLTOJS_BIN) -ppx $(PPX_BIN)
# -dsource is automatically considered by main.byte

ASSEMBLY_BIN := $(GENERATOR_DIR)/assembly.byte
ASSEMBLY     := $(ASSEMBLY_BIN)

DISPLAYGEN_BIN := $(GENERATOR_DIR)/displayed_sources.byte
DISPLAYGEN     := $(OCAMLPAR) $(DISPLAYGEN_BIN)

# Warn if buildtools are missing
$(MLTOJS_BIN) $(LINEOF_BIN) $(PPX_BIN) $(ASSEMBLY_BIN) $(DISPLAYGEN_BIN):
	$(error Missing generator tools, build from project root, or set paths.)

###############################################################
# Dependencies

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
include .depends
endif


###############################################################
# Rules

##### Rule for dependencies
.depends: $(JSREF_ML)
	$(OCAMLDEP) -all $(<D)/* > $@

##### Rule for cmi
%.cmi: %.ml $(MLTOJS_BIN)
	$(MLTOJS) -mode cmi -I $(STDLIB_DIR) $<

%.cmi: %.mli
	ocamlc -I $(STDLIB_DIR) -open Stdlib $<

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

assembly.js: $(ASSEMBLY_JS) $(ASSEMBLY_BIN)
	$(ASSEMBLY) -o $@ $(ASSEMBLY_JS)
# -stdlib $(STDLIB_DIR)/stdlib.js 

##### Rule for displayed_sources.js

displayed_sources.js: $(ALL_DISPLAYED) $(DISPLAYGEN_BIN)
	$(DISPLAYGEN) -o $@ $(ALL_DISPLAYED)


#####################################################################
# Short targets

ALL_TARGETS := lineof.js assembly.js displayed_sources.js
all: $(ALL_TARGETS)

cmi: $(JSREF_ML:.ml=.cmi) $(JSREF_MLI:.mli=.cmi) 

gen: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) $(JSREF_ML:.ml=.token.js)

ref: JsInterpreter.log.js JsInterpreter.unlog.js JsInterpreter.token.js

pseudo: JsInterpreter.pseudo.js JsInterpreter.ptoken.js

log: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.token.js)

unlog: $(JSREF_ML:.ml=.unlog.js) 

lineof: lineof.js

assembly: assembly.js

display: displayed_sources.js

###############################################################
# Global options

.PHONY: all cmi gen ref pseudo log unlog lineof assembly display clean

# Do not delete intermediate files.
.SECONDARY:


#####################################################################
# Clean

DIRTY_EXTS := cmi,{mlloc,log,unlog,pseudo,token,ptoken}.js


clean:
	bash -c "rm -f $(ALL_TARGETS)"
	bash -c "rm -f *.{$(DIRTY_EXTS)}"
	bash -c "rm -f .depends"
