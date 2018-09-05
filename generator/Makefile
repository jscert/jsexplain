OCAMLBUILD  := ocamlbuild -j 4 -classic-display -use-ocamlfind
STDLIB_DIR  := stdlib_ml
EXECUTABLES := main monad_ppx displayed_sources lineof assembly
OCAMLDEP := ocamldep
OCAMLDOT_DIR := ../tools/ocamldot
OCAMLDOT := $(OCAMLDOT_DIR)/ocamldot

default: byte stdlib
all: byte native stdlib
byte:   $(addsuffix .byte,$(EXECUTABLES))
native: $(addsuffix .native,$(EXECUTABLES))

stdlib:
	$(MAKE) -C $(STDLIB_DIR)

# Rules
%.native: FORCE
	$(OCAMLBUILD) $@

%.byte: FORCE
	$(OCAMLBUILD) $@

# Debug
debug:  $(addsuffix .d.byte,$(EXECUTABLES)) .ocamldebug

.ocamldebug: _tags
	grep -o "package([^)]*)" _tags | sed "s/package(\([^)]*\))/\1/" | xargs ocamlfind query -recursive | sed "s/^/directory /" > .ocamldebug

# Archi
ocamldot :
	$(MAKE) -C $(OCAMLDOT_DIR)

archi : ocamldot
	$(OCAMLDEP) *.ml > .depend
	cat .depend | $(OCAMLDOT) | dot -Tpdf > archi_generator.pdf

clean:
	ocamlbuild -clean
	bash -c "rm -f .ocamldebug .depend archi_generator.pdf"
	$(MAKE) -C $(STDLIB_DIR) clean
	$(MAKE) -C $(OCAMLDOT_DIR) clean

.PHONY: default all byte native stdlib debug clean

FORCE: # Force rebuilds of OCaml targets via ocamlbuild, the FORCE file must not exist.
.NOTPARALLEL: # Only one ocamlbuild can be run at a time
