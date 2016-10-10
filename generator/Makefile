OCAMLBUILD  := ocamlbuild -j 4 -classic-display -use-ocamlfind
CC          := ocamlc -c
STDLIB_DIR  := stdlib_ml
EXECUTABLES := main monad_ppx displayed_sources lineof assembly

all: byte native stdlib
byte:   $(addsuffix .byte,$(EXECUTABLES))
native: $(addsuffix .native,$(EXECUTABLES))
stdlib: $(STDLIB_DIR)/stdlib.cmi

# Rules
%.native: FORCE
	$(OCAMLBUILD) $@

%.byte: FORCE
	$(OCAMLBUILD) $@

$(STDLIB_DIR)/stdlib.cmi: $(STDLIB_DIR)/stdlib.mli
	$(CC) $<

# Debug
debug:  $(addsuffix .d.byte,$(EXECUTABLES)) .ocamldebug

.ocamldebug: _tags
	grep -o "package([^)]*)" _tags | sed "s/package(\([^)]*\))/\1/" | xargs ocamlfind query -recursive | sed "s/^/directory /" > .ocamldebug

clean:
	ocamlbuild -clean
	rm -f $(STDLIB_DIR)/*.cmi
	bash -c "rm -f .ocamldebug"

.PHONY: all byte native stdlib debug clean

# Force rebuilds of OCaml targets via ocamlbuild, the FORCE file must not exist.
FORCE:
