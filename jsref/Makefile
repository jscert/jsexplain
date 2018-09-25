#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
###############################################################

all: jsjsref mljsref

###############################################################
##### Tools

GENERATOR_DIR := /home/sam/git/fjs_of_fml/generator

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

ESDOCGEN := ../tools/esdocgen/esdocgen.cmo

# Warn if buildtools are missing
$(MLTOJS_BIN) $(LINEOF_BIN) $(PPX_BIN) $(ASSEMBLY_BIN) $(DISPLAYGEN_BIN) $(ESDOCGEN):
	$(error Missing generator tools, build from project root, or set paths.)

###############################################################
##### Dependencies

# Avoid dependency generation for non-jsjsref build targets
ifeq ($(filter clean%,$(MAKECMDGOALS)),)
include .depends
endif

.depends: $(JSREF_ML) $(PPX_BIN)
	$(OCAMLDEP) -ppx $(PPX_BIN) -I . -all * > $@

###############################################################
###############################################################
# JS-JSRef
JSREF_ML  := $(wildcard *.ml)
JSREF_MLI := $(wildcard *.mli)


STDLIB_DIR := $(GENERATOR_DIR)/stdlib_ml
STDLIB_FLAG := -I $(STDLIB_DIR) -open Stdlib
REPLACE_STDLIB := -nopervasives -nostdlib $(STDLIB_FLAG)

# ASSEMBLY_JS_FILES must respect dependencies order
ASSEMBLY_JS_FILES := \
	Datatypes.unlog.js \
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
	JsInit.unlog.js \
	Prheap.js \
	Debug.js \
	JsInterpreterMonads.unlog.js \
	JsInterpreter.log.js \
	JsInterpreterUtils.unlog.js \
	ModuleExport.js

ASSEMBLY_JS := \
	$(STDLIB_DIR)/stdlib.js \
	$(ASSEMBLY_JS_FILES)

###############################################################

DISPLAYED := \
	JsInterpreter.ml

ALL_DISPLAYED := $(DISPLAYED:.ml=.unlog.js) $(DISPLAYED:.ml=.pseudo.js) $(DISPLAYED)

ALL_LINEOF := $(DISPLAYED:.ml=.token.js) $(DISPLAYED:.ml=.mlloc.js) $(DISPLAYED:.ml=.ptoken.js)



###############################################################
# Rules

##### Rule for cmi
# Ordering of these rules is important, mli rule must be first.
%.cmi: %.mli
	ocamlc $(REPLACE_STDLIB) $(OCAMLCFLAGS) $<

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

OCAMLCFLAGS=

# Strip all extensions of $(ASSEMBLY_JS) and replace with .cmo
ASSEMBLY_ML_FILES := \
	$(addsuffix .cmo,$(basename $(basename $(ASSEMBLY_JS_FILES)))) \
	Run_js.cmo

ASSEMBLY_ML := \
	$(STDLIB_DIR)/stdlib.cmo \
	$(ASSEMBLY_ML_FILES)

mljsref: main.byte

mljsref_debug: OCAMLCFLAGS+=-g
mljsref_debug: mljsref

main.byte: $(ASSEMBLY_ML)
	ocamlfind ocamlc -linkpkg -package JS_Parser -o $@ $(OCAMLCFLAGS) $^

Run_js.cmo: Run_js.ml Run_js.mli Run_js.cmi
	ocamlfind ocamlc -c -package JS_Parser $(OCAMLCFLAGS) $<

Translate_syntax.cmo: Translate_syntax.ml Translate_syntax.mli Translate_syntax.cmi
	ocamlfind ocamlc -c -package JS_Parser $(OCAMLCFLAGS) $<

# Ordering of the .cmo rules is important. mli dependency forces "standard" compilation
%.cmo: %.ml %.mli %.cmi $(PPX_BIN)
	ocamlc -c $(PPX_FLAG) $(OCAMLCFLAGS) $<

# Standalone .ml files
%.cmo: %.ml %.cmi $(PPX_BIN)
	ocamlc -c $(REPLACE_STDLIB) $(PPX_FLAG) $(OCAMLCFLAGS) $<

clean_mljsref:
	rm -f $(ML_FRONTEND_SRC)/*.mli
	rm -f {,$(ML_FRONTEND_SRC)/}*.cmo
	rm -f {,$(ML_FRONTEND_SRC)/}*.byte

.PHONY: mljsref clean_mljsref

##############################################################
##############################################################
# OCamlDocs

DOC_DIR = doc_build

# A dump file is used so indexes are correctly created with separate compilation
# Dump file cannot have a module twice, so must be recreated each run.
DUMP_FILE = $(DOC_DIR)/odoc.dump
DUMP = -dump $(DUMP_FILE) -load $(DUMP_FILE)

ODOC_FLAGS = -html -d $(DOC_DIR) $(DUMP) $(PPX_FLAG) -g $(ESDOCGEN)

doc: $(addprefix doc_build/,$(addsuffix .html,$(basename $(ASSEMBLY_ML_FILES))))
	sh -c "rm -f $(DUMP_FILE)"
	touch $(DOC_DIR)

$(DOC_DIR):
	mkdir -p $@

$(DUMP_FILE): $(DOC_DIR)
	ocamldoc -html -d $(DOC_DIR) -dump $(DUMP_FILE)

$(DOC_DIR)/%.html $(DOC_DIR)/type_%.html: %.ml %.mli %.cmi $(PPX_BIN) $(ESDOCGEN) $(DUMP_FILE)
	ocamlfind ocamldoc $(ODOC_FLAGS) -package JS_Parser $(*F).ml $(*F).mli

$(DOC_DIR)/%.html $(DOC_DIR)/type_%.html: %.ml %.cmi $(PPX_BIN) $(ESDOCGEN) $(DUMP_FILE)
	ocamldoc $(ODOC_FLAGS) $(STDLIB_FLAG) $<

.PHONY: doc

##############################################################
##############################################################
# Global options

clean: clean_jsjsref clean_mljsref
	rm -f *.cmi
	rm -f .depends
	rm -Rf $(DOC_DIR)

# Phony global targets
.PHONY: all clean

.SECONDARY:	# Do not delete intermediate files.
.NOTPARALLEL: 	# Ordered execution expected due to ocamlc writing out cmi files.
